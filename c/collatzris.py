#!/usr/bin/env python
"""Collatzris visualizer  by N. Futatsugi"""

import sys
import os
import curses
import time
import subprocess
import threading
import shlex
import signal

VERSION  = "0.04"
REVISION = "a"
VER_DATE = "20150826"

#VISIBLE = True
VISIBLE = False

class Random:
	def __init__(self, seed):
		self.x = seed

	def __call__(self):
		m = 2**32
		a = 1103515245
		c = 12345
		self.x = (a * self.x + c) % m
		return (self.x >> 16) & 0x7fff

class Command:
	def __init__(self, cmd):
		self.cmd = cmd
		self.process = None

	def run(self, timeout):
		def target():
			self.process = subprocess.Popen(shlex.split(self.cmd), shell=False, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
			self.process.communicate()
		t = threading.Thread(target=target)
		start_time = time.time()
		t.start()
		t.join(timeout)
		runtime = time.time() - start_time
		if t.is_alive():
			self.process.terminate()
			t.join()
		return runtime, self.process.returncode

class Collatzris:
	def __init__(self, seed=0, timewait=0.01):
		self.timewait = timewait
		self.seed = seed
		self.runtime = 0.0
		self.random = Random(seed)
		self.timeout = self.random() % 196 + 5
		self.numbers = self.random() % 1901 + 100
		self.h = self.random() % 81 + 20
		self.w = self.random() % 181 + 20
		self.score = 0
		self.y = 10
		self.x = 10
		if seed == 0:
			self.timeout = 5
			self.numbers = 100
			self.h = 20
			self.w = 20
		self.cnt_numbers = self.numbers
		self.lines = [" " * self.w] * self.h
		self.n = -1
		self.rot = 0
		self.len = 0
		if VISIBLE:
			self.stdscr = curses.initscr()
			self.my, self.mx = self.stdscr.getmaxyx()
			self.pminrow = 0
			self.pmincol = 0
			self.sminrow = 3
			self.smincol = 3
			self.smaxrow = self.my - 1
			self.smaxcol = self.mx - 1
			curses.start_color()
			curses.noecho()
			curses.cbreak()
			self.stdscr.keypad(1)
			curses.init_pair(1, curses.COLOR_GREEN, curses.COLOR_BLACK)
			curses.init_pair(2, curses.COLOR_WHITE, curses.COLOR_BLACK)
			self.pad = curses.newpad(self.h + 2, self.w + 2)
			self.pad.border()
			self.stdscr.addstr(1, 3, "Score: %d" % self.score)
			self.stdscr.refresh()

	def finish(self):
		if VISIBLE:
			curses.nocbreak()
			self.stdscr.keypad(0)
			curses.echo()
			curses.endwin()

	def calc_score(self, n):
		score = 0
		while n > 1:
			n = (n << 1) + n + 1 if n & 1 else n >> 1
			score += 1
		return score

	def draw_field(self):
		for y, line in enumerate(self.lines):
			for x, c in enumerate(line.rstrip()):
				if c.isdigit():
					self.pad.addch(y + 1, x + 1, ord(c), curses.color_pair(2))
		self.pad.refresh(self.pminrow, self.pmincol, self.sminrow, self.smincol, self.smaxrow, self.smaxcol)

	def check_collision(self, y, x, r, l):
		vt = hr = 0
		if r % 2 == 0:
			hr = l // 2
		else:
			vt = l // 2
		if y + vt >= self.h or y - vt < 0 or x + hr >= self.w or x - hr < 0:
			return False
		if r % 2 == 0:
			for px in range(x - hr, x + hr + 1):
				if self.lines[y][px] != " ":
					return False
		else:
			for py in range(y - vt, y + vt + 1):
				if self.lines[py][x] != " ":
					return False
		return True

	def move_number(self, d):
		# clear previous number
		n = "%03d" % self.n
		if VISIBLE:
			if self.rot // 2 == 1:
				n = n[::-1]
			if self.rot % 2 == 0:
				self.pad.addstr(self.y + 1, self.x - self.len // 2 + 1, " " * self.len)
			else:
				for i in range(self.len):
					self.pad.addstr(self.y + i - self.len // 2 + 1, self.x + 1, " ")

			self.draw_field()

		# moves number
		is_next = False
		if d == "D":
			if self.check_collision(self.y + 1, self.x, self.rot, self.len):
				self.y += 1
			else:
				is_next = True
		elif d == "L":
			if self.check_collision(self.y, self.x - 1, self.rot, self.len):
				self.x -= 1
		elif d == "R":
			if self.check_collision(self.y, self.x + 1, self.rot, self.len):
				self.x += 1
		elif d == "C":
			if self.check_collision(self.y, self.x, (self.rot + 1) % 4, self.len):
				prev_rot = self.rot
				self.rot = (self.rot + 1) % 4
				if prev_rot // 2 != self.rot // 2:
					n = n[::-1]

		if VISIBLE:
			if self.rot % 2 == 0:
				self.pad.addstr(self.y + 1, self.x - self.len // 2 + 1, n, curses.color_pair(1))
			else:
				for i in range(self.len):
					self.pad.addstr(self.y + i - self.len // 2 + 1, self.x + 1, n[i], curses.color_pair(1))
			self.pad.move(self.y + 1, self.x + 1)
			self.pad.refresh(self.pminrow, self.pmincol, self.sminrow, self.smincol, self.smaxrow, self.smaxcol)

		return is_next

	def next_number(self):
		self.y = 0
		self.x = self.w // 2
		self.n = self.random() % 1000
		self.len = len("%03d" % self.n)
		self.rot = 0
		self.cnt_numbers -= 1

	def evaluate(self):
		n = "%03d" % self.n
		if self.rot // 2 == 1:
			n = n[::-1]
		if self.rot % 2 == 0:
			sx = self.x - self.len // 2
			gx = self.x + self.len // 2
			s = list(self.lines[self.y])
			for i, x in enumerate(range(sx, gx + 1)):
				s[x] = n[i]
			self.lines[self.y] = "".join(s)
		else:
			sy = self.y - self.len // 2
			gy = self.y + self.len // 2
			for i, y in enumerate(range(sy, gy + 1)):
				s = list(self.lines[y])
				s[self.x] = n[i]
				self.lines[y] = "".join(s)
		number = ""
		cnt = 0
		ids = []
		for i, line in enumerate(self.lines):
			if line.count(" ") == 0:
				number += line
				cnt += 1
				ids.append(i)
		if ids:
			for i in ids[::-1]:
				del self.lines[i]
			for i in range(cnt):
				self.lines.insert(0, " " * self.w)
			self.score += self.calc_score(int(number))
			if VISIBLE:
				self.pad.clear()
				self.pad.border()
				self.stdscr.addstr(1, 3, "Score: %d" % self.score)
				self.stdscr.refresh()

	def execute(self, program, outfile):
		command = Command("%s %d %s" % (program, self.seed, outfile))
		self.runtime, returncode = command.run(timeout=self.timeout)

	def info(self):
		print "seed    : %d" % self.seed
		print "timeout : %d" % self.timeout
		print "height  : %d" % self.h
		print "width   : %d" % self.w
		print "numbers : %d / %d" % (self.cnt_numbers + 1, self.numbers)
		print "runtime : %.3f" % self.runtime
		print "score   : %d" % self.score

	def play(self, moves_file):
		self.next_number()
		self.move_number(".")
		infile = None
		if os.path.isfile(moves_file):
			sz = os.path.getsize(moves_file)
			if sz > 10 * 1024 * 1024:
				self.finish()
				self.info()
				print "filesize: %d" % sz
				return
			infile = open(moves_file)
		if VISIBLE:
			time.sleep(self.timewait)
		MOVE_CHARS = {"J": "L", "L": "R", "K": "D", "I": "C"}
		while True:
			is_next = False
			if infile:
				c = infile.read(1).upper()
				if c == "":
					self.finish()
					self.info()
					infile.close()
					return
				if c not in MOVE_CHARS.values():
					continue
				is_next = self.move_number(c)
				if VISIBLE:
					time.sleep(self.timewait)
			else:
				c = chr(self.pad.getch()).upper()
				if c in MOVE_CHARS:
					is_next = self.move_number(MOVE_CHARS[c])
				elif c == "Q":
					self.finish()
					self.info()
					return
			if is_next:
				self.evaluate()
				self.next_number()
				if self.cnt_numbers < 0 or not self.check_collision(self.y, self.x, self.rot, self.len):
					self.finish()
					self.info()
					return
				self.move_number(".")

def signal_handler(signum, frame):
	if VISIBLE:
		curses.nocbreak()
		curses.echo()
		curses.endwin()
	sys.exit()

def main(args):
	if len(args) < 2:
		print >>sys.stderr, "Usage: %s seed [program=None moves_file=moves.txt timewait=0.01]" % os.path.basename(args[0])
		sys.exit(1)

	seed = int(args[1])
	program = None
	moves_file = ""
	timewait = 0.01
	if len(args) > 2:
		program = args[2]
		moves_file = "moves.txt"
	if len(args) > 3:
		moves_file = args[3]
	if len(args) > 4:
		timewait = float(args[4])

	signal.signal(signal.SIGINT, signal_handler)

	collatzris = Collatzris(seed, timewait)
	if program:
		collatzris.execute(program, moves_file)
	collatzris.play(moves_file)

if __name__ == "__main__": main(sys.argv)
