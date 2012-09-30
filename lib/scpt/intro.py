# Script stolen from Pangband :)

import io

def hidecursor():
	io.set_cursor(0, (79, 23))

def flash(strings, rev):
	colors = [0, 8, 2, 9, 1]

	if rev:
		colors.reverse()

	for i in colors:
		for s in strings:
			x = (80 - len(s[0])) / 2
			io.put_str((s[1], x), s[0], i)
		hidecursor()
		io.fresh()
		io.usleep(50000)

def doubleflash(strings):
	flash(strings, 0)
	hidecursor()
	io.usleep(1000000)
	flash(strings, 1)

def scroll(str, y):
	s1 = str[:len(str) / 2]
	s2 = str[len(str) / 2:]

	for i in range(-len(s1), 40 - len(s1)):
		if i < 0:
			io.put_str((y, 0), s1[0 - i:])
		else:
			io.put_str((y, i), s1)
		io.put_str((y, 79 - i - len(s2)), s2)
		hidecursor()
		io.fresh()
		io.usleep(25000)
		io.erase((y, 0))

	io.put_str((y, (80 - len(str)) / 2), str)

def begin():
	io.clear()
	hidecursor()
	io.fresh()

        line1 = ["Dark God", 8]
	line2 = ["in collaboration with", 10]
        line3 = ["Fingolfin", 12]
	line4 = ["and", 13]
        line5 = ["Eru Iluvatar", 14]

	io.usleep(1000000)
	if io.inkey(1): return
	flash([line1], 0)
	io.usleep(1000000)
	if io.inkey(1): return
	flash([line2], 0)
	io.usleep(1000000)
	if io.inkey(1): return
	flash([line3, line4, line5], 0)
	io.usleep(2500000)
	if io.inkey(1): return
	flash([line1, line2, line3, line4, line5], 1)

	line1 = ["present", 10]

	doubleflash([line1])

	if io.inkey(1): return

        scroll("P E R N A N G B A N D", 10)

	if io.inkey(1): return

	io.usleep(500000)

        line1 = ["P E R N A N G B A N D", 10]
        line2 = ["Pern Angband", 12]

	flash([line2], 0)
	if io.inkey(1): return
	io.usleep(3000000)
	if io.inkey(1): return
	flash([line1, line2], 1)


def curtain():
	strings = []
	for y in range(0, 25):
		str = []
		for x in range(0, 79):
			str.append(io.what((x, y)))
		strings.append(str)
	
	io.clear()

	for y in range(0, 13):
		for x in range(0, 79):
			io.putch((x, 12 - y), strings[12 - y][x])
			io.putch((x, 12 + y), strings[12 + y][x])
		io.fresh()
		io.usleep(30000)

def intro():
	io.clear()
	begin()
	io.clear()
	io.usleep(250000)
