states 14
qs
qa
qb
qc
qa-s
qb-s
qc-s
qfail
qsuccess
qback
qnext
qdone
qr -
qA +
alphabet 10 a b c a. b. c. a+ b+ c+ #
qs a qa a+ R
qs b qb b+ R
qs c qc c+ R
qs # qr # R
qs _ qr _ R
q1 a qa a. R
q1 b qb b. R
q1 c qc c. R
q1 # qA # R
q1 _ qr _ R
qa a qa a R
qa b qa b R
qa c qa c R
qa # qa-s # R
qa _ qr _ R
qb a qb a R
qb b qb b R
qb c qb c R
qb # qb-s # R
qb _ qr _ R
qc a qc a R
qc b qc b R
qc c qc c R
qc # qc-s # R
qc _ qr _ R
qa-s a qsuccess a. L
qa-s b qfail b. L
qa-s c qfail c. L
qa-s a. qa-s a. R
qa-s b. qa-s b. R
qa-s c. qa-s c. R
qa-s # qr # R
qa-s _ qr # R
qb-s a qfail a. L
qb-s b qsuccess b. L
qb-s c qfail c. L
qb-s a. qb-s a. R
qb-s b. qb-s b. R
qb-s c. qb-s c. R
qb-s # qr # R
qb-s _ qr # R
qc-s a qfail a. L
qc-s b qfail b. L
qc-s c qsuccess c. L
qc-s a. qc-s a. R
qc-s b. qc-s b. R
qc-s c. qc-s c. R
qc-s # qr # R
qc-s _ qr # R
qfail a qfail a L
qfail b qfail b L
qfail c qfail c L
qfail a. qfail a. L
qfail b. qfail b. L
qfail c. qfail c. L
qfail # qback # L
qfail _ qr _ R
qback a qback a L
qback b qback b L
qback c qback c L
qback a. qback a L
qback b. qback b L
qback c. qback c L
qback a+ qa a+ R
qback b+ qb b+ R
qback c+ qc c+ R
qback # qr # R
qback _ qr _ R
qsuccess a qsuccess a L
qsuccess b qsuccess b L
qsuccess c qsuccess c L
qsuccess a. qsuccess a. L
qsuccess b. qsuccess b. L
qsuccess c. qsuccess c. L 
qsuccess # qnext # L
qnext a qnext a L
qnext b qnext b L
qnext c qnext c L
qnext a. q1 a. R
qnext b. q1 b. R
qnext c. q1 b. R
qnext a+ q1 a+ R
qnext b+ q1 b+ R
qnext c+ q1 b+ R
qnext # qdone # R
qdone a qA a R
qdone b qA b R
qdone c qA c R
qdone a. qdone a. R
qdone b. qdone b. R
qdone c. qdone c. R
qdone _ qA _ L