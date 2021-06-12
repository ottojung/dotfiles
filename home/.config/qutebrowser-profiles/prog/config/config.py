# Load YML config file
config.load_autoconfig()

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'file://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome://*/*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'qute://*/*')

# auto_save session
# Type: Bool
# Default: True
config.set('auto_save.session', True)

#############
## PLUGINS ##
#############

from qutebrowser.api import cmdutils

@cmdutils.register(instance='command-dispatcher', scope='window', overwrite=True)
def reload_really(self):
	''' Issue "open" with current url '''
	url = self._yank_url('url')
	return self.openurl(url=url)

import note_save
