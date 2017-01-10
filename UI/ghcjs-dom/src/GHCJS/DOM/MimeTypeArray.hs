{-# LANGUAGE CPP #-}
module GHCJS.DOM.MimeTypeArray (
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
  module GHCJS.DOM.JSFFI.Generated.MimeTypeArray
#else
  module Graphics.UI.Gtk.WebKit.DOM.MimeTypeArray
#endif
  ) where
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import GHCJS.DOM.JSFFI.Generated.MimeTypeArray
#else
import Graphics.UI.Gtk.WebKit.DOM.MimeTypeArray
#endif
