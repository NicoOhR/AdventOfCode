def expand(s):
    map = ""
    for idx,c in enumerate(s):
        if idx%2 == 0:
            map = map + (str(int(idx/2)) * int(c))
        else:
            map = map + ('.' * int(c))
    return map

def defrag(s):
    last = len(s) - 1 
    map = list(s)
    for idx in range(len(map)):
        if map[idx] == '.':
            while last > idx and map[last] == '.':
                last -= 1
            if last > idx:
                map[last], map[idx] = map[idx], map[last]
                last -= 1
    return ''.join(map)

def checksum(s):
    total = 0
    map = list(s)
    for idx, c in enumerate(map):
        if(c != '.'):
            total += (idx * int(c))
    return total

def main():
    with open("input.txt", "r") as file:
        contents = file.read()
        contents = contents = contents.replace("\n", "")
    print(checksum(defrag(expand(contents))))

if __name__ == "__main__":
    main()
