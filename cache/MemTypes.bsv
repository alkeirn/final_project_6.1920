typedef Bit#(32) LineAddr; //check if this should be 26
typedef struct { Bit#(1) write; LineAddr addr; Bit#(32) data; } MainMemReq deriving (Eq, FShow, Bits, Bounded);
typedef Bit#(32) MainMemResp;
typedef struct { Bit#(32) first; Bit#(32) second; Bit#(1) second_valid;} CacheResp deriving (Eq, FShow, Bits, Bounded);
typedef enum {Ready, StartMiss, SendFillReq, WaitFillResp, GetHit} ReqStatus deriving (Eq, Bits);
typedef struct {Bit#(7) idx; Bit#(21) addr; MainMemResp data;} Store deriving (Eq, FShow, Bits, Bounded);

typedef Bit#(32) Word;
