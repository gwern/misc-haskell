{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- For deriving Enum

-- module Nuke (main)
--    where
-- TODO: work in radiation deaths.

import Test.QuickCheck
import Control.Monad (liftM3)
import Data.List (sort)

-- Test apparatus
main :: IO ()
main = kpaTest >> psiTest >> energyTest >> thermalMaxTest >> timeMaxTest >> kpaTest >> return ()

deepCheck :: (Testable prop) => prop -> IO Bool
deepCheck = quickCheckWith 1000 2 1000000

-- | Test whether a function monotonically increases, like many of these should.
increasing :: (Enum a, Num a, Ord b) => (a -> b) -> a -> Bool
increasing s l = let m = map s [1..l] in (sort m == m)

{- | For many equations and results, it is nonsensical to have negative results, but we don't want
to use solely natural numbers because then we lose precision. So we define a PosReal type which tries
to define the subset of real numbers which are 0 or positive; this way the type system checks for negative
results instead of every other function having conditionals checking for negative input or output. -}
newtype PosReal = MakePosReal Float deriving (Show, Eq, Ord, Enum)

-- | Basic numerical operations on positive reals
instance Num PosReal where
    fromInteger = toPosReal . fromInteger
    x + y = MakePosReal (fromPosReal x + fromPosReal y)
    x - y = toPosReal ((fromPosReal x) - (fromPosReal y))
    x * y = MakePosReal (fromPosReal x * fromPosReal y)
    abs x | x >= 0 = x
          | otherwise = x * (-1)
    signum x | x >= 0 = 1
             | otherwise = (-1)

-- | Define division on PosReals
instance Fractional PosReal where
    x / y = toPosReal ((fromPosReal x) / (fromPosReal y))
    fromRational x = MakePosReal (fromRational x)

-- | Note that positive reals are truncated at 0.
toPosReal :: Float -> PosReal
toPosReal x
    | x < 0     = MakePosReal 0
    | otherwise = MakePosReal x
fromPosReal :: PosReal -> Float
fromPosReal (MakePosReal i) = i

-- | Define an instance to allow QuickCheck operations
instance Arbitrary PosReal where
    arbitrary = liftM3 fraction arbitrary arbitrary arbitrary
        where fraction :: Integer -> Integer -> Integer -> PosReal
              fraction a b c = fromInteger a + (fromInteger b / (abs (fromInteger c) + 1))

type KiloPascal = PosReal
type PSI = PosReal
type Meters = PosReal
type W = PosReal -- ^ Note W is always in kilotons.
type Joule = PosReal
type Time = PosReal

-- Characterizes the output of a nuclear weapon
data Output = Output {
      heat :: PosReal,
      blast :: PosReal,
      nuclear_radiation :: PosReal } deriving (Show, Eq)

-- Units
kpaTest :: IO Bool
kpaTest = deepCheck (\s -> kpa s >= 0 && increasing kpa s)
kpa :: KiloPascal -> PSI
kpa a
    | (a > 0) = 0.145 * a
    | otherwise = 0

psiTest :: IO Bool
psiTest = deepCheck (\s -> psi s >= s && increasing psi s)
psi :: PSI -> KiloPascal
psi = (6.895 *)

meter :: PosReal -> PosReal
meter = (3.281 *)

foot :: PosReal -> PosReal
foot = (0.3048 *)

-- | Find ideal height for an air burst.
heightTest :: IO Bool
heightTest = deepCheck (\s -> height s > 0 && increasing height s)
height :: W -> Float
--height w = 60 * (1 / (w * w * w)) -- w^3
height w | w < 0.5   = 1
         | otherwise = 60 * ((fromPosReal w)**(1/3))

{- | Calculate total energy of a given kilotonnage. Answer in joules.
   This works because critical mass gives a lower kilotonnage bound for fission bombs. -}
energyTest :: IO Bool
energyTest = deepCheck (\s -> energy s == 0 || energy s >= 2.0929999e12 && increasing energy s)
energy :: W -> Joule
energy s
    | s >= 0.5 = 4.186 * joule * s
    | otherwise = 0
         where joule = 1000000000000 :: PosReal -- 10^12

regularNuke :: W -> Output
regularNuke w = Output { heat=(0.35 * energy w), -- 35% heat
                         blast=(0.50 * energy w), -- 50% kinetic energy
                         nuclear_radiation=(0.15 * energy w) -- 15% non-thermal radiation
                       }

{- | A radiation weapon trades off a decreased thermal and blast energy against a considerably
   increased radiation yield. It is defined in terms of regularNuke because it just modifies it. -}
radiationNuke :: W -> Output
radiationNuke w = Output {
                    -- regularNuke w
                    heat=(0.35 * energy (0.50 * w)),
                    blast=(0.50 * energy (0.50 * w)),
                    nuclear_radiation=(0.15 * energy (10.0 * w)) } -- 10x non-thermal radiation!

{- | Answer in joules /m^2. r = distance from impact/radius; t = correction factor (1.5^2 for snow & high
   clouds, 1.5 for singly either). This is how many joules of heat per square inch at a given distance for
   given kilotonnage (weather roughly included). -}
totalPointImpact :: W -> Meters -> PosReal -> PosReal -> Joule
totalPointImpact w r clouds snow
    | r /= 0 = (0.35 * (weatherMultiplier snow clouds) * (energy w)) / (4.0 * reducedPrecisionPi * r)
    | otherwise = energy w -- If 0, then at ground zero and no diminution b/c of distance
    where
      weatherMultiplier :: PosReal -> PosReal -> PosReal
      weatherMultiplier a b
          | a*b > 0 = (a * b)
          | a + b > 0 = (a `max` b)
          | otherwise = 1
      reducedPrecisionPi = 3.141592653589793 :: PosReal

-- Results in kPA
peakOverPressureUnderWater :: W -> Meters -> Joule
peakOverPressureUnderWater w r = 1.07e7 * (1 / (w / 0.37)) * (1 / (r / (-1.18))) -- = 1.07e7 * w^(0.37) * r^(-1.18)

-- thermal deaths - pmax = kJ/m^2
--thermalMax = 270 -- incapacitation
thermalMaxTest :: IO Bool
thermalMaxTest = deepCheck (\s -> (thermalMax s s s s) >= 0)
thermalMax :: Meters -> Joule -> Joule -> Joule -> Joule
thermalMax w r clouds snow = (0.38 * (totalPointImpact w r clouds snow)) / timeMax w

timeMaxTest :: IO Bool
timeMaxTest = deepCheck (\s -> timeMax s >= 0)
timeMax :: W -> Time
timeMax w = 0.0417 * (1 / (w / 0.44))

-- TODO: add a test
pressureKilledPerson :: Meters -> Joule -> Joule -> Joule -> Bool
pressureKilledPerson w r clouds snow
                     | thermalMax w r clouds snow >= 270 = True
                     | otherwise = False

--radiationDeaths = dose-rate * exposure-time
-- radiationKilled = undefined
-- centigray is 1/10 a gray
--gray joule kg = joule / kg
--450 centigray = 50% fatalities
-- We'll assume the following scales linearly
-- at 10 r, 3billion / 1million major defects 1stgen (1/3000th), /10million (1/300th) cumulative total
--          3billion / 10million (1/300th) early mortalities 1stgen, /200million (1/15th) cumulative total

{- weapon vital statistics
--
icbms
--
minuteman II
# 450
11 300
1 W56

minuteman III
# 200
13 000
3 W62

minuteman III (MK12A)
# 300
13 000
3 W78

mx
50
11 000
10 W87
--
slbm
--
poseidon
224
4 600
10 w68

trident I
384
7 400
8 w76
--
bombers
--
B-1B
97
9 800
22

FB-111A
59
4 700
6

B-52G/H
193
16 000
b052h/g model: 20 SRAM or
b-52h: 20 ALCM
b-52g:  12 ALCM and 6 bombs

f-4 C/D/E
2 250
1060-2400
3

F-15 A/C
5 W25 or genies

F-16 A/B/C/D
5 B43 or B57

F-111 A/D/E/F
3 b43|b57|b61|b83
--
Missiles
--
pershing II
111
1 790
1 W85

GLCM
250
2 500
1 W84

Pershing IA
72
740
1 W50

lance
100
125
1 W70

nike hercules
27
160
1 W31

tomahawk
200
2 500
1 W80-0
--
bombs
--
W31
75
1-20kt

W56
450
1.2MT

W50
100
60-400

W62
600
170kt

W70
1 282
1-100kt

W78
900
335kt

W87
500
300kt

W68
2 240
40kt

w76
3 072
100kt

w80-0
200
5-150kt

w85
125
.3-80

w84
325
.2-150kt

b28
b28RE
b43
b57
b61
b83 "genie"

"b-52G/H 20 SRAM"
B-52G
B-52H

-}
{-
A list of possible scenarios.

US first strike
USSR first strike
NATO / Warsaw Pact
Far East strategy
US USSR escalation

Middle East war
USSR - China attack
India Pakistan war
Mediterranean war
Hong Kong variant

SEATO decapitating
Cuban provocation
Inadvertent
Atlantic heavy
Cuban paramilitary

Nicaraguan preemptive
Pacific territorial
Burmese theatre-wide
Turkish decoy
NATO first strike

Argentina escalation
Iceland maximum
Arabian theatre-wide
U.S. subversion
Australian maneuver

Iranian diversion
...? limited
Sudan surprise
NATO territorial
Zaire alliance

Iceland incident
English escalation
Zaire sudden
Egypt paramilitary
Middle East heavy

Mexican takeover
Chad alert
Saudi maneuver
African territorial
Ethiopian escalation

Canadian ...?
Turkish heavy
NATO incursion
U.S. defense
Cambodian heavy

Pact medium
Arctic minimal
Mexican domestic
Taiwan theatre-wide
Pacific maneuver

Portugal revolution
Albanian decoy
Palestinian local
Moroccan minimal
Hungarian diversion

Czech option
French alliance
Arabian clandestine
Gabon rebellion
Northern maximum

Syrian surprise
...?sh paramilitary
SEATO takeover
Hawaiian escalation
Iranian maneuver

NATO containment
Swiss incident
Cuban minimal
Chad alert
Iceland escalation

Vietnamese retaliation
Syrian provocation
Libyan local
Gabon takeover
Romanian war

Middle East offensive
Denmark massive
Chile confrontation
S.African subversion
USSR alert

Nicaraguan thrust
Greenland domestic
Iceland heavy
Kenya option
Pacific defense

Uganda maximum
Thai subversion
Romanian strike
Pakistan sovereignty
Afghan misdirection

Thai variation
Northern territorial
Polish paramilitary
S.African offensive
Panama misdirection

Scandinavian domestic
Jordan preemptive
English thrust
Burmese maneuver
Spain counter

Arabian offensive
Chad interdiction
Taiwan misdirection
Bangladesh theatre-wide
Ethiopian local

Italian takeover
Vietnamese incident
English preemptive
Denmark alternate
Thai confrontation

Taiwan surprise
Brazilian strike
Venezuela sudden
Malaysian alert
Israel discretionary

Libyan action
Palestinian tactical
NATO alternate
Cyprus maneuver
Egypt misdirection

Bangladesh thrust
Kenya defense
Bangladesh containment
Vietnamese strike
Albanian containment

Gabon surprise
Iraq sovereignty
Vietnamese sudden
Lebanon interdiction
Taiwan domestic

Algerian sovereignty
Arabian strike
Atlantic sudden
Mongolian thrust
Polish decoy

Alaskan discretionary
Canadian thrust
Arabian light
S.African domestic
Tunisian incident

Malaysian maneuver
Jamaica decoy
Malaysian minimal
Russian sovereignty
Chad option

Bangladesh war
Burmese containment
Asian theatre-wide
Bulgarian clandestine
Greenland incursion

Egypt surgical
Czech heavy
Taiwan confrontation
Greenland maximum
Uganda offensive

Caspian defense
-}
