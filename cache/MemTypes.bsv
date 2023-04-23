typedef Bit#(26) LineAddr;
typedef struct { Bit#(1) write; LineAddr addr; Bit#(32) data; } MainMemReq deriving (Eq, FShow, Bits, Bounded);
typedef Bit#(32) MainMemResp;
typedef enum {Ready, StartMiss, SendFillReq, WaitFillResp, GetHit} ReqStatus deriving (Eq, Bits);
typedef struct {Bit#(7) idx; Bit#(21) addr; MainMemResp data;} Store deriving (Eq, FShow, Bits, Bounded);

typedef Bit#(32) Word;
