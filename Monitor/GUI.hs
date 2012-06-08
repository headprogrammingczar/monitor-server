module Monitor.GUI where

import Graphics.UI.Gtk

type Callback object callback = (ConnectId object, Signal object callback)

