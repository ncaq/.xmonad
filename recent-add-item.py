#!/usr/bin/env python3

import os.path
import sys

import gi
gi.require_version('Gtk', '3.0')

from gi.repository import GObject
from gi.repository import Gtk

Gtk.RecentManager.get_default().add_item(
    'file://' + os.path.abspath(sys.argv[1]))
GObject.idle_add(Gtk.main_quit)
Gtk.main()
