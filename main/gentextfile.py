teststr = "a @fn(s32 s32) 32 32.10 32.0000 @ s body body body body s\n"

with open('example0.txt', 'w') as file:
    for i in range(0, 1):
        file.write(teststr)

with open('example1.txt', 'w') as file:
    for i in range(0, 10_000_000):
        file.write(teststr)
