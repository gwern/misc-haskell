module Sandbox (chroot, runAsNobody) where

import Foreign.C
import System.Directory (setCurrentDirectory)

foreign import ccall "unistd.h chroot" chrootC :: CString -> IO Int

chroot :: String -> IO ()
chroot dir = do throwErrnoIfMinus1_ "chroot" (withCString dir chrootC)
                setCurrentDirectory "/"

{- http://www.ibm.com/developerworks/linux/library/l-sppriv.html
http://www.dwheeler.com/secure-programs/Secure-Programs-HOWTO/minimize-privileges.html
http://www.unixwiz.net/techtips/chroot-practices.html
http://www.isso.sparta.com/opensource/privman/
http://hackage.haskell.org/trac/ghc/ticket/1380
http://www.pphsg.org/safeghc/

TODO: write a Lambdabot patch to remove core files and limit memory usage. Suggest disallowing file access

Useful functions to keep in mind:

System.Posix.User:
setUserID :: UserID -> IO ()
setGroupID :: GroupID -> IO ()

System.Posix.Temp:
mkstemp :: String -> IO (String, Handle)
mkstemp - make a unique filename and open it for reading/writing (only safe on GHC & Hugs)

System.Posix.Process:
executeFile :: FilePath -> Bool -> [String] -> Maybe [(String, String)] -> IO ()
forkProcess :: IO () -> IO ProcessID
nice :: Int -> IO ()


Model of dropping privileges:
main = stuff


stuff :: IO () -> IO () -> IO () -> PrivSep ()
stuff =    1 :: Sudo () -- Use setuid, setgid to change to root to do whatever is needed
           2 :: User () -- do stuff as current user
           3 :: Nobody () -- chroot somewhere like /var/empty; use setuid, setgid to change to nobody, and do everything else

stuff would have a higher order signature; take three IO functions; so the first would be an IO function doing everything necessary as root; 1 = setuid 0 >> delete "foobar" >> setCpuPerf 100

The second would be as user: 2 = foobar <- readFile "~/foo"; and the third as nobody: 3 = chroot /var/emtpy >> echo foobar;

Thoughts: the PrivSep monad should enforce everything to be of three types: Sudo, User, Nobody (high privileges, normal, and low).

Everything referentially transparent or running user-accessible binaries could be in Nobody.

Everything reading from files owned by the user could be in User

Everything requiring root would be in Root, obviously.

Model of privilege separation:

privSep :: (String -> String) -> Sudo ()

The idea is that we somehow how to fork off a process, and have this process escalate to root, and run some specified functions.

We could do other things like ulimit and nice the non-privileged processes.


Alternative approach:
See <http://haskell.org/haskellwiki/Safely_running_untrusted_Haskell_code> for running in the Nobody monad, use all of Lambdabot's security guarantees:
'Lambdabot uses 1) type guarantee of no-IO at the top level, along with
2) a trusted module base (pure module only, that are trusted to not
export evil things), as well as 3) restricting only to H98-language only
(things like TH can, and have been, exploited, for example).'

Lambdabot uses most of these techniques in RunPlugs.hs

But it can only do these things, apparently, because it compiles everything passed to it in a known context. If a function, and not a string, is passed in there is no way to compile it in the known context and so guarantee that problematic libraries and particularly the unsafe functions are not included.



-}
