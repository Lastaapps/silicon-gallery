module Consts where

import qualified Data.Text as T

domain :: T.Text
domain = "www.siliconhill.cz"

baseUrl :: T.Text
baseUrl = "https://" <> domain

baseUrlHttp :: T.Text
baseUrlHttp = "http://" <> domain

siliconHillRed :: Int
siliconHillRed = 0xDA251D

postsAuthor :: T.Text
postsAuthor = "Silicon Gallery"

postsFootnote :: T.Text
postsFootnote = "Fotogalerie Silicon Hillu"

urlPhotogalleryPrefix :: T.Text
urlPhotogalleryPrefix = "/photogalleries"
