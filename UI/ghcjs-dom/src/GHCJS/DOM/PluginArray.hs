{-# LANGUAGE CPP #-}
module GHCJS.DOM.PluginArray (
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
  module GHCJS.DOM.JSFFI.Generated.PluginArray
#else
  module Graphics.UI.Gtk.WebKit.DOM.PluginArray
#endif
  ) where
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import GHCJS.DOM.JSFFI.Generated.PluginArray
#else
import Graphics.UI.Gtk.WebKit.DOM.PluginArray
#endif
