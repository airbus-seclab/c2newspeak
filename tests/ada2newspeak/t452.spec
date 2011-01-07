Newspeak output
---------------
void t452.gg(void) {
  (t452.adb:3#9)^int32 x;
  (t452.adb:4#14)^uint1 msg_id;
  (t452.adb:4#14)^msg_id =(uint1) 0;
  (t452.adb:6#10)^choose {
   -->
    (t452.adb:6#10)^guard((msg_id_uint1 ==_uint1 t452c.c_acm_uint1));
    (t452.adb:7#33)^x =(int32) 1;
   -->
    (t452.adb:6#10)^choose {
     -->
      (t452.adb:6#10)^guard((msg_id_uint1 ==_uint1 t452c.m_acm_uint1));
     -->
      (t452.adb:6#10)^guard(! (msg_id_uint1 ==_uint1 t452c.m_acm_uint1));
      (t452.adb:6#10)^guard(! (msg_id_uint1 ==_uint1 t452c.c_acm_uint1));
    }
  }
}

uint1 t452c.c_acm;
uint1 t452c.m_acm;

