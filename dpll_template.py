import sys

def read_cnf_input():
    """Reads a cnf formula from standard input. Literals are represented by int's,
    clauses are lists of literals, and the formula is a list of clauses.
    
    You are free to assume that the input is syntactically correct, as described
    in the coursework document.
    
    >>> read_cnf_input() # where the input is given by the coursework example
    [[-1, 2], [-1, -2, 3], [1], [-3]]
    
    """
    header = sys.stdin.readline().split()
    n = int(header[2])
    m = int(header[3])
    clauses = []
    for i in range(m):
        clause = map(int, sys.stdin.readline().split()) # get a line of ints
        clause.pop() # remove the last 0
        clauses.append(clause)
    return clauses

def find_unit(clauses):
    """Tries to find a unit clause in the given set of clauses. If it finds a unit
    clause, it returns the single literal in the clause; otherwise, if there are no
    unit clauses, it returns None.
    
    >>> find_unit([[1, -2, 3], [3], [4, 5]])
    3
    
    >>> find_unit([[1, 3], [-4, 5]])
    None
    
    """
    return None

def simplify_unit(literal, clauses):
    """Simplifies the given set of clauses assuming that the selected literal is true.
    This performs a single step of unit propagation.
    
    >>> simplify_unit(-1, [[1, 3], [-1, 2, 4], [-3, 4, 5], [-1]])
    [[3], [-3, 4, 5]]
    
    >>> simplify_unit(2, [[1, -2, -3], [2], [-2]])
    [[1, -3], []]
    
    """
    return []

def unit_propagate(clauses):
    """Takes a set of clauses and repeatedly applies unit propagation until it no
    longer contains unit clauses.
    
    >>> unit_propagate([[1, 3], [-1, 2, 4], [-3, 4, 5], [-1]])
    [[4, 5]]
    
    >>> unit_propagate([[1, -2, -3], [2], [-2]])
    [[1, -3], []]

    """
    return []

def contains_empty_clause(clauses):
    """Returns true if the given set of clauses contains an empty clause.
    
    >>> contains_empty_clause([[-4, -5], [3], [1, -2, 3]])
    False
    
    >>> contains_empty_clause([[1, 3], []])
    True
    
    """
    return False

def pick_literal(clauses):
    """Select any literal occurring in the set of clauses. You can assume that the set
    of clauses is not empty, and that it does not contain empty clauses.
    
    Any literal in the formula would work. You could, for example, pick the first literal
    of the first clause. But you could also try other strategies.
    
    >>> pick_literal([[-4, -5], [1, -2, 3]])
    -4
    
    """
    return clauses[0][0]

def dpll(clauses):
    """Returns true if the given set of clauses is satisfiable, and false otherwise.
    
    >>> dpll([[-1, 2], [-1, -2, 3], [1], [-3]])
    False
    
    >>> dpll([[1, 2], [1, -3, -5], [-1, 2, -4], [-1, 3]])
    True
    
    """
    return True

def main():
    clauses = read_cnf_input()
    if dpll(clauses):
        print "satisfiable"
    else:
        print "unsatisfiable"

if __name__ == "__main__":
    main()
