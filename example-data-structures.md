
# Input
n=3
1 Ross
2 Monica
3 Chandler
4 Phoebe
5 Joey
6 Rachel

1: 6 4 2
2: 3 5 1
3: 2 6 4
4: 5 1 3
5: 6 4 2
6: 1 5 3

# Parsed data-structures

    Proposers = Array(
        Proposer(id:1, label: Ross, prefs: Stack(6,4,2)),
        Proposer(id:3, label: Chandler, prefs: Stack(2,6,4)),
        Proposer(id:5, label: Joey, prefs: Stack(6,4,2))
    )
    
    // Note!: Prefs is a lookup from (floor(id/2) -> p) where larger p is better
    // and p <= n
    Responders = Array(
        Responder(id:2, label: Monica, prefs: Array(1,3,2)),
        Responder(id:4, label: Phoebe, prefs: Array(2,1,3)),
        Responder(id:6, label: Racher, prefs: Array(3,1,2))
    )
    
    // Note: When the algorithm runs, Proposers is transformed to a Queue