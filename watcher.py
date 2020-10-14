import os.path, time
files = ['interpreter.ss', 'datatypes.ss', 'enviornment.ss', 'parser.ss']
changes =  {
	"interpreter.ss":os.path.getmtime("interpreter.ss"),
	"datatypes.ss":os.path.getmtime("datatypes.ss"),
	"enviornment.ss":os.path.getmtime("enviornment.ss"),
	"parser.ss":os.path.getmtime("parser.ss")
}
changes = { file : os.path.getmtime(file) for file in files }
while True:
	for f in files:
		if changes.get(f) < os.path.getmtime(f):
			print (time.strftime("%H:%M:%S", time.localtime()))
			with open('turnMeIn.ss', 'w') as outfile:
				for fname in files:
					with open(fname) as infile:
						for line in infile:
							outfile.write(line)
	changes = { file : os.path.getmtime(file) for file in files }
	time.sleep(1)