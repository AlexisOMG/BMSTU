import sys

MATCH_SCORE = 0
MISMATCH_SCORE = -1
GAP_SCORE = -2

class Cluster:
    def __init__(self, first=None, second=None, id=None, dist=None) -> None:
        self.first = first
        self.second = second
        self.id = id
        if id is not None:
            self.height = 0
            self.size = 1
        else:
            self.height = dist / 2
            self.size = first.size + second.size

    def __str__(self):
        if self.id is not None:
            return str(self.id)
        return f"({self.first},{self.second}:{self.height})"

    def __repr__(self) -> str:
        return str(self)


def solve(file):
    seqs = parse_fasta(file)
    clusters = [Cluster(None, None, i) for i in range(len(seqs))]
    matrix = {}
    for i in range(len(clusters)):
        for j in range(len(clusters)):
            if i != j:
                matrix[(clusters[i], clusters[j])] = -calc_memory_optimized(seqs[i], seqs[j], MATCH_SCORE, MISMATCH_SCORE, GAP_SCORE)
    # seqs = ['a', 'b', 'c', 'd', 'e']
    # arr = [
    #     [0, 17, 21, 31, 23],
    #     [17, 0, 30, 34, 21],
    #     [21, 30, 0, 28, 39],
    #     [31, 34, 28, 0, 43],
    #     [23, 21, 39, 43, 0],
    #     ]
    # for i in range(5):
    #     for j in range(5):
    #         if i != j:
    #             matrix[(clusters[i], clusters[j])] = arr[i][j]
    while len(matrix) > 0:
        mn = float('inf')
        mn_key = None
        for key in matrix:
            if matrix[key] < mn:
                mn = matrix[key]
                mn_key = key
        first, second = mn_key
        c = Cluster(first, second, None, mn)
        clusters = [x for x in clusters if x != first and x != second]
        res = {}
        for a in clusters:
            for b in clusters:
                if a != b:
                    res[(a,b)] = matrix[(a,b)]
        clusters.append(c)
        for x in clusters:
            if x != c:
                res[(x, c)] = (first.size * matrix[(first, x)] + second.size * matrix[(second, x)]) / (first.size + second.size)
                res[(c, x)] = res[(x, c)]
        matrix = res
    print(clusters[0])
    
def parse_fasta(file):
    with open(file) as f:
        lines = f.readlines()
    cur = ""
    res = []
    for line in lines:
        if line.startswith(">"):
            res.append(cur)
            cur = ""
        else:
            cur += line.strip()
    res.append(cur)
    return res[1:]

def calc_memory_optimized(s1, s2, match_score, mismatch_score, gap_score):
    n = len(s1)
    m = len(s2)
    prev = [0] * (m + 1)
    cur = [i * gap_score for i in range(m + 1)]

    for i in range(1, n + 1):
        prev = cur
        cur = [0] * (m + 1)
        cur[0] = i * gap_score
        for j in range(1, m + 1):
            if s1[i - 1] == s2[j - 1]:
                cur_score = match_score
            else:
                cur_score = mismatch_score
            a = prev[j - 1] + cur_score
            b = prev[j] + gap_score
            c = cur[j - 1] + gap_score
            cur[j] = max(a, b, c)
    return cur[m]


if __name__ == '__main__':
    solve(sys.argv[1])