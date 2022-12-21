from typing import Callable, Tuple

DEBUG = False

def score_fun(a: str, 
              b: str,
              match_score: int = 5, 
              mismatch_score: int = -4) -> int:
    return match_score if a == b else mismatch_score

def needleman_wunsch_affine(seq1: str, 
                            seq2: str, 
                            score_fun: Callable = score_fun, 
                            gap_open: int = -10, 
                            gap_extend: int = -1) -> Tuple[str, str, int]:
    '''
    Inputs:
    seq1 - first sequence
    seq2 - second sequence
    score_fun - function that takes two characters and returns score
    gap_open - gap open penalty
    gap_extend - gap extend penalty
    Outputs:
    aln1 - first aligned sequence
    aln2 - second aligned sequence
    score - score of the alignment
    '''

    #infinity = 2 * gap_open + (n + m - 2) * gap_extend + 1
    infinity = float('-inf')

    # 1. Initialize matrices
    n = len(seq1)+1
    m = len(seq2)+1

    M = [[0 for _ in range(m)] for _ in range(n)]
    I = [[0 for _ in range(m)] for _ in range(n)]
    D = [[0 for _ in range(m)] for _ in range(n)]
    SCORE = [[[0, 'D'] for _ in range(m)] for _ in range(n)]
    I[0][0], D[0][0] = infinity, infinity
    for i in range(1, n):
        M[i][0] = infinity
        I[i][0] = gap_open+(i-1)*gap_extend
        D[i][0] = infinity
        SCORE[i][0] = [I[i][0], 'D']

    for j in range(1, m):
        M[0][j] = infinity
        I[0][j] = infinity
        D[0][j] = gap_open+(j-1)*gap_extend
        SCORE[0][j] = [D[0][j], 'I']

    


    # 2. Fill matrices
    # We assume that consecutive gaps on different sequences are not allowed
    for i in range(1, n):
        for j in range(1, m):
            cost = score_fun(seq1[i-1], seq2[j-1])
            M[i][j] = max(
                M[i-1][j-1],
                I[i-1][j-1],
                D[i-1][j-1]
            ) + cost
            
            I[i][j] = max(
                I[i][j - 1] + gap_extend, 
                M[i][j - 1] + gap_open
            )
            
            D[i][j] = max(
                D[i - 1][j] + gap_extend, 
                M[i - 1][j] + gap_open
            )
            
            ma = max(M[i][j], I[i][j], D[i][j])
            if ma == D[i][j]:
                SCORE[i][j] = [ma, 'D']
            elif ma == M[i][j]:
                SCORE[i][j] = [ma, 'M']
            elif ma == I[i][j]:
                SCORE[i][j] = [ma, 'I']
            


    # 3. Traceback
    aln1 = ''
    aln2 = ''
    i = len(seq1)
    j = len(seq2)
    while i > 0 or j > 0:
        if SCORE[i][j][1] == 'D':
            aln1 = seq1[i-1] + aln1
            aln2 = "-" + aln2
            i -= 1
        elif SCORE[i][j][1] == 'I':
            aln1 = "-" + aln1
            aln2 = seq2[j-1] + aln2
            j -= 1
        elif SCORE[i][j][1] == 'M':
            aln1 = seq1[i-1] + aln1
            aln2 = seq2[j-1] + aln2
            i-=1
            j-=1

    
    return aln1, aln2, SCORE[n-1][m-1][0]

def print_array(matrix: list):
    for row in matrix:
        for element in row:
            print(f"{element:6}", end="")
        print()

def main():
    aln1, aln2, score = needleman_wunsch_affine("ACGT", "TAGT", gap_open=-10, gap_extend=-1) 
    print(f'str 1: {aln1}')
    print(f'str 2: {aln2}')
    print(f'score: {score}')
    


if __name__ == "__main__":
    main()