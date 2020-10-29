import os.path, time
files = ['datatypes.ss', 'enviornment.ss', 'interpreter.ss', 'parser.ss']
changes = { file : os.path.getmtime(file) for file in files }
while True:
	for f in files:
		if changes.get(f) < os.path.getmtime(f):
			print (time.strftime("%H:%M:%S", time.localtime()))
			with open('17.ss', 'w') as outfile:
				for fname in files:
					with open(fname) as infile:
						for line in infile:
							outfile.write(line)
	changes = { file : os.path.getmtime(file) for file in files }
	time.sleep(1)
