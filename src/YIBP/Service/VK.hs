module YIBP.Service.VK where

import Data.Text qualified as T

import YIBP.VK.Client
import YIBP.VK.Types

getNameByPeerId :: VKClient -> Maybe Int -> Int -> IO T.Text
getNameByPeerId = undefined
