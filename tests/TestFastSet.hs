module Main (main) where

import Data.Word (Word8)
import qualified Data.Attoparsec.ByteString.FastSet as F
import qualified Data.Attoparsec.Text.FastSet as F
import Test.QuickCheck

prop_members s =
    let set = F.fromList s
    in all (`F.memberWord8` set) s
    
prop_nonmembers s s' =
    let set = F.fromList s
    in not . any (`F.memberWord8` set) $ filter (not . (`elem` s)) s'
    
prop_membersText s =
    let set = F.fromList s
    in all (`FT.member` set) s
    
prop_nonmembersText s s' =
    let set = FT.fromList s
    in not . any (`FT.member` set) $ filter (not . (`elem` s)) s'
    
main = do
  quickCheck prop_members
  quickCheck prop_nonmembers
  quickCheck prop_membersText
  quickCheck prop_nonmembersText
