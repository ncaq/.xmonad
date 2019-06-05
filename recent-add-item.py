#!/usr/bin/env python3

from gi.repository import Gtk
from gi.repository import GObject
import os.path
import sys
import gi

gi.require_version('Gtk', '3.0')

Gtk.RecentManager.get_default().add_item(
    'file://' + os.path.abspath(sys.argv[1]))
GObject.idle_add(Gtk.main_quit)
Gtk.main()
