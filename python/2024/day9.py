def expand(s):
    map = []
    for idx,c in enumerate(s):
        if idx%2 == 0:
            map += [int(idx/2) for _ in range(int(c))]
        else:
            map += [-1 for _ in range(int(c))]
    return map

def defrag(map):
    last = len(map) - 1 
    for idx in range(len(map)):
        if map[idx] == -1:
            while last > idx and map[last] == -1:
                last -= 1
            if last > idx:
                map[last], map[idx] = map[idx], map[last]
                last -= 1
    return map

def checksum(map):
    total = 0
    for idx, c in enumerate(map):
        if(c != -1):
            total += (idx * int(c))
    return total

def main():
    with open("input.txt", "r") as file:
        contents = file.read()
        contents = contents = contents.replace("\n", "")

    print(checksum(defrag(expand(contents))))

if __name__ == "__main__":
    main()
