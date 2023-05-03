typedef Bit#(32) LineAddr; 
typedef struct { Bit#(1) write; LineAddr addr; Bit#(512) data; } MainMemReq deriving (Eq, FShow, Bits, Bounded);
typedef Bit#(512) MainMemResp;
typedef struct { Bit#(32) first; Bit#(32) second; Bool second_valid;} CacheResp deriving (Eq, FShow, Bits, Bounded);
typedef enum {Ready, StartMiss, SendFillReq, WaitFillResp, GetHit} ReqStatus deriving (Eq, Bits);
typedef struct {Bit#(7) idx; Bit#(21) addr; MainMemResp data;} Store deriving (Eq, FShow, Bits, Bounded);
typedef Bit#(32) Word;
typedef struct { Bit#(4) byte_en; Bit#(32) addr; Bit#(32) data; } Mem deriving (Eq, FShow, Bits, Bounded);
