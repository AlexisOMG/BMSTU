import time
from blosum import BLOSUM
from enum import IntEnum
import numpy as np
import multiprocessing

k = 2
gap = -10
score_min = 150
min_diag_wieght = 50
min_diag_score = 100
max_gap_cnt = 5
blos = BLOSUM(62)


def find_k_mers(s: str, k: int) -> dict:
  """Return a dictionary of k-mers and their indexes in a string."""
  res={}
  for i in range(len(s)-k+1):
    if s[i:i+k] in res:
      res[s[i:i+k]].append(i)
    else:
      res[s[i:i+k]]=[i]
  return res


def find_diff(s: str, kmers: dict, k: int, bias: int) -> dict:
  """Return a list of dictionaries of k-mers and their diag indexes in a string."""
  idx = {}
  for i in range(len(s)-k+1):
    for j in kmers:
      if j not in idx:
        idx[j] = {}
      if s[i:i+k] in kmers[j]:
        for ind in kmers[j][s[i:i+k]]:
          diff = ind - i
          if diff not in idx[j]:
            idx[j][diff] = []
          idx[j][diff].append(((i, i+k), (ind, ind+k)))
          # idx[j][diff] = sorted(idx[j][diff])
  # cnt = 0
  res = {}
  for j in kmers:
    if j not in res:
      res[j] = {}
    for d in idx[j]:
      if len(idx[j][d]) > bias:
        # print(j, d)
        res[j][d] = sorted(idx[j][d])
  return res


def filter_diffs(idx: dict, bias: int) -> dict:
  """Return a list of dictionaries of k-mers and their diag indexes in a string."""
  res1 = {}
  for i in idx:
    if i not in res1:
      res1[i] = {}
    for key, value in idx[i].items():
      if len(value) > bias:
        # res[i][key] = value
        res1[i][key] = idx[i][key]
  return res1


def score(a: str, b: str):
  return sum([blos[a[i]+b[i]] for i in range(len(a))])


def calc_diag_score(s: str, idx: dict, bias: int, k: int) -> dict:
  res1 = {}
  for i in idx:
    if i not in res1:
      res1[i] = {}
    for key, values in idx[i].items():
      tmp = [values[0]]
      bscore = 0
      
      for j in range(1, len(values)):
        # print(values[j])
        # print(tmp[-1])
        a = score(s[values[j][0][0]:values[j][0][1]], i[values[j][1][0]:values[j][1][1]])
        # print(a)
        b = score(s[tmp[-1][0][0]:tmp[-1][0][1]], i[tmp[-1][1][0]:tmp[-1][1][1]])
        # print(b)
        c = score(s[tmp[-1][0][0]:values[j][0][1]], i[tmp[-1][1][0]:values[j][1][1]])
        # print(c)
        if a+b < c:
          tmp[-1] = ((tmp[-1][0][0], values[j][0][1]), (tmp[-1][1][0], values[j][1][1]))
          bscore += c
        else:
          bscore += a+b
          tmp.append(values[j])
      # for j in range(min_i, max_i):
      if bscore > bias:
        # res[i][key] = bscore
        res1[i][key] = sorted(tmp)
  return res1


# Assigning the constants for the scores
class Score(IntEnum):
    MATCH = 1
    MISMATCH = -1
    GAP = -1

# Assigning the constant values for the traceback
class Trace(IntEnum):
    STOP = 0
    LEFT = 1 
    UP = 2
    DIAGONAL = 3

# Implementing the Smith Waterman local alignment
def smith_waterman(seq1, seq2):
    # Generating the empty matrices for storing scores and tracing
    row = len(seq1) + 1
    col = len(seq2) + 1
    matrix = np.zeros(shape=(row, col), dtype=int)  
    tracing_matrix = np.zeros(shape=(row, col), dtype=int)  
    
    # Initialising the variables to find the highest scoring cell
    max_score = -1
    max_index = (-1, -1)
    
    # Calculating the scores for all cells in the matrix
    for i in range(1, row):
        for j in range(1, col):
            # Calculating the diagonal score (match score)
            match_value = blos[seq1[i - 1]+seq2[j - 1]]
            # match_value = Score.MATCH if seq1[i - 1] == seq2[j - 1] else Score.MISMATCH
            diagonal_score = matrix[i - 1, j - 1] + match_value
            
            # Calculating the vertical gap score
            vertical_score = matrix[i - 1, j] + gap
            
            # Calculating the horizontal gap score
            horizontal_score = matrix[i, j - 1] + gap
            
            # Taking the highest score 
            matrix[i, j] = max(0, diagonal_score, vertical_score, horizontal_score)
            
            # Tracking where the cell's value is coming from    
            if matrix[i, j] == 0: 
                tracing_matrix[i, j] = Trace.STOP
                
            elif matrix[i, j] == horizontal_score: 
                tracing_matrix[i, j] = Trace.LEFT
                
            elif matrix[i, j] == vertical_score: 
                tracing_matrix[i, j] = Trace.UP
                
            elif matrix[i, j] == diagonal_score: 
                tracing_matrix[i, j] = Trace.DIAGONAL 
                
            # Tracking the cell with the maximum score
            if matrix[i, j] >= max_score:
                max_index = (i,j)
                max_score = matrix[i, j]
    
    # Initialising the variables for tracing
    aligned_seq1 = ""
    aligned_seq2 = ""   
    current_aligned_seq1 = ""   
    current_aligned_seq2 = ""  
    (max_i, max_j) = max_index
    
    # Tracing and computing the pathway with the local alignment
    while tracing_matrix[max_i, max_j] != Trace.STOP:
        if tracing_matrix[max_i, max_j] == Trace.DIAGONAL:
            current_aligned_seq1 = seq1[max_i - 1]
            current_aligned_seq2 = seq2[max_j - 1]
            max_i = max_i - 1
            max_j = max_j - 1
            
        elif tracing_matrix[max_i, max_j] == Trace.UP:
            current_aligned_seq1 = seq1[max_i - 1]
            current_aligned_seq2 = '-'
            max_i = max_i - 1    
            
        elif tracing_matrix[max_i, max_j] == Trace.LEFT:
            current_aligned_seq1 = '-'
            current_aligned_seq2 = seq2[max_j - 1]
            max_j = max_j - 1
            
        aligned_seq1 = aligned_seq1 + current_aligned_seq1
        aligned_seq2 = aligned_seq2 + current_aligned_seq2
    
    # Reversing the order of the sequences
    aligned_seq1 = aligned_seq1[::-1]
    aligned_seq2 = aligned_seq2[::-1]
    
    return aligned_seq1, aligned_seq2, max_score


def solve(s1: str, s2: str, idx: dict, n: int):
  stop = False
  tmp = {}
  for key1 in idx:
    for v1 in idx[key1]:
      tmp[v1] = 1

  while not stop:
    stop = True
    # for key1 in idx:
    #   for key2 in idx:
    #     if key1 == key2:
    #       continue
    news = []
    for v1 in tmp:
      for v2 in tmp:
        if v1 == v2:
          continue
        if v1[0][0] == v2[0][1]:
          if abs(v1[0][0] - v2[0][1]) < n:
            new_i = ((v2[0][0], v1[0][1]), (min(v2[1][0], v1[1][0]), max(v1[1][1], v2[1][1])))
            news.append(new_i)
        elif v2[0][0] == v1[0][1]:
          if abs(v2[0][0] - v1[0][1]) < n:
            new_i = ((v1[0][0], v2[0][1]), (min(v1[1][0], v2[1][0]), max(v2[1][1], v1[1][1])))
            news.append(new_i)
        elif v1[1][0] == v2[1][1]:
          if abs(v1[1][0] - v2[1][1]) < n:
            new_i = ((min(v2[0][0], v1[0][0]), max(v1[0][1], v2[0][1])), (v2[1][0], v1[1][1]))
            news.append(new_i)
        elif v2[1][0] == v1[1][1]:
          if abs(v2[1][0] - v1[1][1]) < n:
            new_i = ((min(v1[0][0], v2[0][0]), max(v2[0][1], v1[0][1])), (v1[1][0], v2[1][1]))
            news.append(new_i)
    for new_i in news:
      if new_i not in tmp:
        tmp[new_i] = 1
        stop = False
  a, b, m = '', '', -1
  for t in tmp:
    s1_r, s2_r, m1 = smith_waterman(s1[t[0][0]:t[0][1]], s2[t[1][0]:t[1][1]])
    if m1 > m:
      a = s1_r
      b = s2_r
      m = m1
  
  return a, b, m


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


lines = parse_fasta('uniprot_sprot.fasta')
# lines = parse_fasta('small.fasta')
s = 'MFVFLVLLPLVSSQCVNLTTRTQLPPAYTNSFTRGVYYPDKVFRSSVLHSTQDLFLPFFSNVTWFHAIHVSGTNGTKRFDNPVLPFNDGVYFASTEKSNIIRGWIFGTTLDSKTQSLLIVNNATNVVIKVCEFQFCNDPFLGVYYHKNNKSWMESEFRVYSSANNCTFEYVSQPFLMDLEGKQGNFKNLREFVFKNIDGYFKIYSKHTPINLVRDLPQGFSALEPLVDLPIGINITRFQTLLALHRSYLTPGDSSSGWTAGAAAYYVGYLQPRTFLLKYNENGTITDAVDCALDPLSETKCTLKSFTVEKGIYQTSNFRVQPTESIVRFPNITNLCPFGEVFNATRFASVYAWNRKRISNCVADYSVLYNSASFSTFKCYGVSPTKLNDLCFTNVYADSFVIRGDEVRQIAPGQTGKIADYNYKLPDDFTGCVIAWNSNNLDSKVGGNYNYLYRLFRKSNLKPFERDISTEIYQAGSTPCNGVEGFNCYFPLQSYGFQPTNGVGYQPYRVVVLSFELLHAPATVCGPKKSTNLVKNKCVNFNFNGLTGTGVLTESNKKFLPFQQFGRDIADTTDAVRDPQTLEILDITPCSFGGVSVITPGTNTSNQVAVLYQDVNCTEVPVAIHADQLTPTWRVYSTGSNVFQTRAGCLIGAEHVNNSYECDIPIGAGICASYQTQTNSPRRARSVASQSIIAYTMSLGAENSVAYSNNSIAIPTNFTISVTTEILPVSMTKTSVDCTMYICGDSTECSNLLLQYGSFCTQLNRALTGIAVEQDKNTQEVFAQVKQIYKTPPIKDFGGFNFSQILPDPSKPSKRSFIEDLLFNKVTLADAGFIKQYGDCLGDIAARDLICAQKFNGLTVLPPLLTDEMIAQYTSALLAGTITSGWTFGAGAALQIPFAMQMAYRFNGIGVTQNVLYENQKLIANQFNSAIGKIQDSLSSTASALGKLQDVVNQNAQALNTLVKQLSSNFGAISSVLNDILSRLDKVEAEVQIDRLITGRLQSLQTYVTQQLIRAAEIRASANLAATKMSECVLGQSKRVDFCGKGYHLMSFPQSAPHGVVFLHVTYVPAQEKNFTTAPAICHDGKAHFPREGVFVSNGTHWFVTQRNFYEPQIITTDNTFVSGNCDVVIGIVNNTVYDPLQPELDSFKEELDKYFKNHTSPDVDLGDISGINASVVNIQKEIDRLNEVAKNLNESLIDLQELGKYEQYIKWPWYIWLGFIAGLIAIVMVTIMLCCMTSCCSCLKGCCSCGSCCKFDEDDSEPVLKGVKLHYT'
# s = 'CATCTGACTCCT'
# s= 'QVQLVQSGAEVKKPGSSVKVSCKASGGTFSNYAISWVRQAPGQGLEWMGRIIPILGIANYAQKFQGRVTITADKSTSTAYMELSSLRSEDTAVYYCARGYYEARHYYYYYAMDVWGQGTAVTVSSAS'
batch_size = 1000
res = []
i = 0


def process(id: int, start: int, s: str, lines: list, k: int, batch_size: int, score_min: int, q):
  i = 0
  while i <= len(lines)-batch_size:
    print(f'Process {id}, step {i}')
    kmers = {}
    for l in lines[i:i+batch_size]:
      kmers[l] = find_k_mers(l, k)
    idx = find_diff(s, kmers, k, min_diag_wieght)
    # idx = filter_diffs(idx, 1)
    idx = calc_diag_score(s, idx, min_diag_score, k)
    # print(len(idx))
    cnt = 0
    for l in lines[i:i+batch_size]:
      a, b, m = solve(s, l, idx[l], max_gap_cnt)
      if m > score_min:
        q.put((a, b, m, start+i+cnt))
      cnt += 1
    i+=batch_size


q = multiprocessing.Queue()
prs = []
size = 50000
id = 0
for i in range(0, len(lines), size):
  id += 1
  prs.append(multiprocessing.Process(target=process, args=(id, i, s, lines[i:i+size], k, batch_size, score_min, q)))

start_time = time.time()
for i in range(len(prs)):
  prs[i].start()

for i in range(len(prs)):
  prs[i].join()
res = []
while q.empty() is False:
  res.append(q.get())

print(res)

print("--- %s seconds ---" % (time.time() - start_time))
