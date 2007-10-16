{- module Main where
main = val <- nobodyFunction stuff (getArgs))
       print val

stuff = shellScripting
-}

module Nobody where
import System.Eval.Haskell      (unsafeEval_)
import System.Posix.Resource    (setResourceLimit,
                                 Resource(..),
                                 ResourceLimits(ResourceLimits),
                                 ResourceLimit(ResourceLimit))
import System.Posix.Process (nice)
import Monad (liftM)
import System.Random

--nobodyFunction :: (a1 -> t) -> a1 -> IO t
nobodyFunction :: [Char] -> [Char] -> IO (Either [String] a)
nobodyFunction f b = do
--                      limitResources
--                     chroot
--                     cwd
--                     setuid; setgid;
--                      x <- sequence (take 3 (repeat $ getStdRandom (randomR (97,122)) >>= return . chr))
                      s <- unsafeEval_ ("let foo = " ++ f ++ b ++ "\n foo") limitContext ["-O","-fasm","-fextended-default-rules"] [] []
                      return (s)

limitResources :: IO ()
limitResources = do
                    setResourceLimit ResourceCPUTime $ ResourceLimits (ResourceLimit 100) (ResourceLimit 1000)
                    -- Core files can be used for DOS attacks on free disk space. Disallow them
                    setResourceLimit ResourceCoreFileSize $ ResourceLimits none none
                    -- I don't even have a gig of physical memory anyway
                    setResourceLimit ResourceDataSize $ ResourceLimits halfgig gig
                    -- No shell needs a tebibyte sized file. Even a gigabyte is  pushing it.
                    setResourceLimit ResourceFileSize $ ResourceLimits softFile hardFile
                    -- Let's disallow file IO!
                    setResourceLimit ResourceOpenFiles $ ResourceLimits none none
                   -- setResourceLimit ResourceStackSize -- Not really a concern
                    setResourceLimit ResourceTotalMemory $ ResourceLimits halfgig gig
                    nice 10 -- nice it below user's default, but not all the way to 19
                     where none = ResourceLimit 0
                           gig = ResourceLimit 1073741824
                           halfgig = ResourceLimit 536870912
--                           tebibyte = ResourceLimit 1099511627776
                           softFile = ResourceLimit 7000000
                           hardFile = ResourceLimit 68730328


limitContext = prelude ++ prehier ++ datas ++ qualifieds ++ controls ++ other ++ template ++ extras
                   where prelude =
                             ["qualified Prelude as P", "Prelude"]
                         other   =
                             ["Text.Printf"
                             ,"Text.PrettyPrint.HughesPJ"]
                         prehier =
                             ["Char", "List", "Maybe", "Numeric", "Random" ]
                         qualifieds =
                             ["qualified Data.Map                    as M"
                             ,"qualified Data.IntMap                 as I"
                             ,"qualified Data.ByteString             as S"
                             ,"qualified Data.ByteString.Char8       as SC"
                             ,"qualified Data.ByteString.Lazy        as L"
                             ,"qualified Data.ByteString.Lazy.Char8  as LC"
                             ,"qualified Data.Set"
                             ,"qualified Data.Generics"
                             ,"qualified Data.IntSet"
                             ,"qualified Data.Foldable"
                             ,"qualified Data.Sequence"
                             ,"qualified Data.Traversable"
                             ,"qualified Control.Monad.Writer"
                             ]
                         datas   = map ("Data." ++)
                                   ["Array"
                                   ,"Bits"
                                   ,"Bool"
                                   ,"Char"
                                   ,"Complex"
                                   ,"Dynamic"
                                   ,"Either"
                                   ,"Eq"
                                   ,"Fixed"
                                   --  ,"Foldable"
                                   --  ,"Function"
                                   --  ,"Generics"
                                   ,"Graph"
                                   ,"Int"
                                   --  ,"IntMap"
                                   --  ,"IntSet"
                                   ,"Ix"
                                   ,"List"
                                   --  ,"Map"
                                   ,"Maybe"
                                   ,"Monoid"
                                   ,"Ord"
                                   ,"Ratio"
                                   --  ,"Set"
                                   ,"Tree"
                                   ,"Tuple"
                                   ,"Typeable"
                                   ,"Word"
                                   ]
                         controls = map ("Control." ++)
                                    ["Monad"
                                    ,"Monad.Cont"
                                    ,"Monad.Error"
                                    ,"Monad.Identity"
                                    ,"Monad.List"
                                    ,"Monad.RWS"
                                    ,"Monad.Reader"
                                    ,"Monad.State"
                                    ,"Monad.Trans"
                                    ,"Monad.Fix"
                                    ,"Monad.Instances"
                                    ,"Applicative"
                                    ,"Arrow"
                                    --  ,"Arrow.Transformer"
                                    --  ,"Arrow.Transformer.All"
                                    --  ,"Arrow.Operations"
                                    ,"Parallel"
                                    ,"Parallel.Strategies"
                                    ]
                         template = [] -- ["Language.Haskell.TH hiding (runIO,reify)"]
                         extras   = ["ShowQ","ShowFun","L","LargeWord"]
