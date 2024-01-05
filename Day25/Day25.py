import collections as C


G = C.defaultdict(set)

for line in open('test.txt'):
    u, *vs = line.replace(':','').split()
    for v in vs: G[u].add(v); G[v].add(u)

print(G)

S = set(G)

count = lambda v: len(G[v]-S)

while sum(map(count, S)) != 3:
    print(*map(count, S))
    S.remove(max(S, key=count))


print(len(S) * len(set(G)-S))

