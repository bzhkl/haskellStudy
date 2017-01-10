{-# LANGUAGE CPP #-}
module GHCJS.DOM.HTMLSelectElement (
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
  module GHCJS.DOM.JSFFI.Generated.HTMLSelectElement
#else
  module Graphics.UI.Gtk.WebKit.DOM.HTMLSelectElement
#endif
  ) where
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import GHCJS.DOM.JSFFI.Generated.HTMLSelectElement
#else
import Graphics.UI.Gtk.WebKit.DOM.HTMLSelectElement
#endif
