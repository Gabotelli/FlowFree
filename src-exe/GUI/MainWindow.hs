module GUI.MainWindow
(MainWindow)
where

import GI.Gtk as Gtk
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

data MainWindow = MainWindow{
	mwWindow :: !ApplicationWindow
}

initMainWindow :: (Monad m) => m MainWindow
initMainWindow builder = do
				builder <- builderNewFromResource "/gui/src-exe/Flow.glade"

				window <- getObject builder "App" ApplicationWindow

				let gui = MainWindow { mwWindow = window }

				pure gui


getObject
	:: (MonadIO m, GObject o)
	=> Gtk.Builder
	-> Text
	-> (ManagedPtr o -> o)
	-> m o
getObject builder obj gtkConstr = do
	o <- builderGetObject builder obj
	case o of
		Nothing ->
			error $ "Could not get object " <> show obj <> " in Glade"
		Just oo -> do
			w <- liftIo $ castTo gtkConstr oo
			case w of
				Nothing -> error $ "GTK: cannot cast widget " <> show obj
				Just widget -> return widget