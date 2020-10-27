{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module RSS where

import Parser
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Renderer.Text
import qualified Data.Text.Lazy as TL

feed :: Markup -> Markup
feed = customParent "feed"
title :: TL.Text -> Markup
title = customParent "title" . text . TL.toStrict
link :: TL.Text -> Markup
link = customParent "link" . text . TL.toStrict
updated :: TL.Text -> Markup
updated = customParent "updated" . text . TL.toStrict
author' :: TL.Text -> Markup
author' = customParent "author" . customParent "name" . text . TL.toStrict
id' :: TL.Text -> Markup
id' = customParent "id" . text . TL.toStrict
entry :: Markup -> Markup
entry = customParent "entry"
summary :: TL.Text -> Markup
summary = customParent "summary" . text . TL.toStrict

atom :: [Message] -> TL.Text
atom messages = renderMarkup $ do
    customParent "feed" $ do
        title "Haskell Café"
        link "https://mail.haskell.org/pipermail/haskell-cafe/"
        updated "TODO"
        foldMap messageToEntry messages

messageToEntry :: Message -> Markup
messageToEntry (Message{..}) = do
    entry $ do
        title subject
        -- link $
        id' messageID
        updated date
        summary $ TL.take 60 content


-- <?xml version="1.0" encoding="utf-8"?>
-- <feed xmlns="http://www.w3.org/2005/Atom">

--   <title>Example Feed</title>
--   <link href="http://example.org/"/>
--   <updated>2003-12-13T18:30:02Z</updated>
--   <author>
--     <name>John Doe</name>
--   </author>
--   <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>

--   <entry>
--     <title>Atom-Powered Robots Run Amok</title>
--     <link href="http://example.org/2003/12/13/atom03"/>
--     <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
--     <updated>2003-12-13T18:30:02Z</updated>
--     <summary>Some text.</summary>
--   </entry>

-- </feed>
