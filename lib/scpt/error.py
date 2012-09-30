# Error

import sys
from io import msg_print

class ErrorClass:
	def write(self, text):
		error_file = open("/error.txt", "a")
		error_file.write(text)
		error_file.close()
		if (text[-1] == '\n'):
			msg_print(text[0:-1])
		else:
			msg_print(text)

sys.stderr = sys.stdout = ErrorClass()

