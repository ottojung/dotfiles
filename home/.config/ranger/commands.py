from ranger.api.commands import *

import os

class open_in_terminal(Command):

    def execute(self):
        if self.arg(1):
            target_filename = self.rest(1)
        else:
            target_filename = self.fm.thisfile.path

        target_dir = os.path.dirname(target_filename)
        os.system('gnome-terminal --working-directory="{}"'.format(target_dir))

