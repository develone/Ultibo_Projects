#!/bin/bash
rm -f *.o exPointers exRecord rijndael-test rijndael-tool
/home/devel/ultibo/core/fpc/bin/fpc -B -Tlinux -Parm @/home/devel/ultibo/core/fpc/bin/fpc.cfg -O2 rijndael-test
/home/devel/ultibo/core/fpc/bin/fpc -B -Tlinux -Parm @/home/devel/ultibo/core/fpc/bin/fpc.cfg -O2 rijndael-tool
/home/devel/ultibo/core/fpc/bin/fpc -B -Tlinux -Parm @/home/devel/ultibo/core/fpc/bin/fpc.cfg -O2 exPointers
/home/devel/ultibo/core/fpc/bin/fpc -B -Tlinux -Parm @/home/devel/ultibo/core/fpc/bin/fpc.cfg -O2 exRecord
