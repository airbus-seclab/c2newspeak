Warning: 620.c:32#1116: goto statement accepted
Warning: 620.c:34#1138: goto statement accepted
Newspeak output
---------------
620.c
void main(void) {
  (620.c:27#6)^uint32 goto.lbl;
  (620.c:27#6)^0- =(uint32) 0;
  (620.c:27#6)^{
    int32 i;
    (620.c:28#2)^1- =(uint32) 1-_uint32;
    (620.c:28#2)^{
      uint32 switch.620.c:28#2.1;
      (620.c:28#2)^choose {
       -->
        (620.c:28#2)^guard(! 2-_uint32);
        (620.c:28#2)^0- =(uint32) coerce[0,4294967295] 1-_int32;
       -->
        (620.c:28#2)^guard(2-_uint32);
        (620.c:28#2)^0- =(uint32) 0;
      }
      (620.c:28#2)^2- =(uint32) 2-_uint32;
      (620.c:28#2)^{
        uint32 switch.620.c:28#2.0;
        (620.c:28#2)^choose {
         -->
          (620.c:28#2)^guard(! 3-_uint32);
          (620.c:28#2)^0- =(uint32) 1-_uint32;
         -->
          (620.c:28#2)^guard(3-_uint32);
          (620.c:28#2)^0- =(uint32) 0;
        }
        (620.c:28#2)^do {
          (620.c:33#2)^do {
            (620.c:31#2)^do {
              (620.c:29#2)^do {
                (620.c:28#2)^choose {
                 -->
                  (620.c:28#2)^guard((0-_uint32 ==_int32 0));
                  (620.c:29#2)^goto lbl8;
                 -->
                  (620.c:28#2)^choose {
                   -->
                    (620.c:28#2)^guard((0-_uint32 ==_int32 1));
                    (620.c:31#2)^goto lbl7;
                   -->
                    (620.c:28#2)^choose {
                     -->
                      (620.c:28#2)^guard((0-_uint32 ==_int32 2));
                      (620.c:33#2)^goto lbl6;
                     -->
                      (620.c:28#2)^guard(! (0-_uint32 ==_int32 2));
                      (620.c:28#2)^guard(! (0-_uint32 ==_int32 1));
                      (620.c:28#2)^guard(! (0-_uint32 ==_int32 0));
                      (620.c:28#2)^goto lbl5;
                    }
                  }
                }
              } with lbl8: {
              }
              (620.c:30#7)^goto lbl5;
            } with lbl7: {
            }
            (620.c:32#2)^3- =(uint32) 1;
            (620.c:32#2)^choose {
             -->
              (620.c:32#2)^guard(3-_uint32);
              (620.c:32#2)^goto lbl5;
             -->
              (620.c:32#2)^guard(! 3-_uint32);
            }
          } with lbl6: {
          }
          (620.c:34#2)^3- =(uint32) 1;
          (620.c:34#2)^choose {
           -->
            (620.c:34#2)^guard(3-_uint32);
            (620.c:34#2)^goto lbl5;
           -->
            (620.c:34#2)^guard(! 3-_uint32);
          }
        } with lbl5: {
        }
        (620.c:28#2)^3- =(uint32) 3-_uint32;
      }
      (620.c:28#2)^do {
        (620.c:28#2)^while (1) {
          (620.c:28#2)^choose {
           -->
            (620.c:28#2)^guard(2-_uint32);
           -->
            (620.c:28#2)^guard(! 2-_uint32);
            (620.c:28#2)^goto lbl3;
          }
          (620.c:28#2)^2- =(uint32) 2-_uint32;
          (620.c:28#2)^{
            uint32 switch.620.c:28#2.0;
            (620.c:28#2)^choose {
             -->
              (620.c:28#2)^guard(! 3-_uint32);
              (620.c:28#2)^0- =(uint32) 1-_uint32;
             -->
              (620.c:28#2)^guard(3-_uint32);
              (620.c:28#2)^0- =(uint32) 0;
            }
            (620.c:28#2)^do {
              (620.c:33#2)^do {
                (620.c:31#2)^do {
                  (620.c:29#2)^do {
                    (620.c:28#2)^choose {
                     -->
                      (620.c:28#2)^guard((0-_uint32 ==_int32 0));
                      (620.c:29#2)^goto lbl14;
                     -->
                      (620.c:28#2)^choose {
                       -->
                        (620.c:28#2)^guard((0-_uint32 ==_int32 1));
                        (620.c:31#2)^goto lbl13;
                       -->
                        (620.c:28#2)^choose {
                         -->
                          (620.c:28#2)^guard((0-_uint32 ==_int32 2));
                          (620.c:33#2)^goto lbl12;
                         -->
                          (620.c:28#2)^guard(! (0-_uint32 ==_int32 2));
                          (620.c:28#2)^guard(! (0-_uint32 ==_int32 1));
                          (620.c:28#2)^guard(! (0-_uint32 ==_int32 0));
                          (620.c:28#2)^goto lbl11;
                        }
                      }
                    }
                  } with lbl14: {
                  }
                  (620.c:30#7)^goto lbl11;
                } with lbl13: {
                }
                (620.c:32#2)^3- =(uint32) 1;
                (620.c:32#2)^choose {
                 -->
                  (620.c:32#2)^guard(3-_uint32);
                  (620.c:32#2)^goto lbl11;
                 -->
                  (620.c:32#2)^guard(! 3-_uint32);
                }
              } with lbl12: {
              }
              (620.c:34#2)^3- =(uint32) 1;
              (620.c:34#2)^choose {
               -->
                (620.c:34#2)^guard(3-_uint32);
                (620.c:34#2)^goto lbl11;
               -->
                (620.c:34#2)^guard(! 3-_uint32);
              }
            } with lbl11: {
            }
            (620.c:28#2)^3- =(uint32) 3-_uint32;
          }
        }
      } with lbl3: {
      }
      (620.c:28#2)^2- =(uint32) 2-_uint32;
    }
    (620.c:28#2)^do {
      (620.c:28#2)^while (1) {
        (620.c:28#2)^choose {
         -->
          (620.c:28#2)^guard(1-_uint32);
         -->
          (620.c:28#2)^guard(! 1-_uint32);
          (620.c:28#2)^goto lbl1;
        }
        (620.c:28#2)^1- =(uint32) 1-_uint32;
        (620.c:28#2)^{
          uint32 switch.620.c:28#2.1;
          (620.c:28#2)^choose {
           -->
            (620.c:28#2)^guard(! 2-_uint32);
            (620.c:28#2)^0- =(uint32) coerce[0,4294967295] 1-_int32;
           -->
            (620.c:28#2)^guard(2-_uint32);
            (620.c:28#2)^0- =(uint32) 0;
          }
          (620.c:28#2)^2- =(uint32) 2-_uint32;
          (620.c:28#2)^{
            uint32 switch.620.c:28#2.0;
            (620.c:28#2)^choose {
             -->
              (620.c:28#2)^guard(! 3-_uint32);
              (620.c:28#2)^0- =(uint32) 1-_uint32;
             -->
              (620.c:28#2)^guard(3-_uint32);
              (620.c:28#2)^0- =(uint32) 0;
            }
            (620.c:28#2)^do {
              (620.c:33#2)^do {
                (620.c:31#2)^do {
                  (620.c:29#2)^do {
                    (620.c:28#2)^choose {
                     -->
                      (620.c:28#2)^guard((0-_uint32 ==_int32 0));
                      (620.c:29#2)^goto lbl22;
                     -->
                      (620.c:28#2)^choose {
                       -->
                        (620.c:28#2)^guard((0-_uint32 ==_int32 1));
                        (620.c:31#2)^goto lbl21;
                       -->
                        (620.c:28#2)^choose {
                         -->
                          (620.c:28#2)^guard((0-_uint32 ==_int32 2));
                          (620.c:33#2)^goto lbl20;
                         -->
                          (620.c:28#2)^guard(! (0-_uint32 ==_int32 2));
                          (620.c:28#2)^guard(! (0-_uint32 ==_int32 1));
                          (620.c:28#2)^guard(! (0-_uint32 ==_int32 0));
                          (620.c:28#2)^goto lbl19;
                        }
                      }
                    }
                  } with lbl22: {
                  }
                  (620.c:30#7)^goto lbl19;
                } with lbl21: {
                }
                (620.c:32#2)^3- =(uint32) 1;
                (620.c:32#2)^choose {
                 -->
                  (620.c:32#2)^guard(3-_uint32);
                  (620.c:32#2)^goto lbl19;
                 -->
                  (620.c:32#2)^guard(! 3-_uint32);
                }
              } with lbl20: {
              }
              (620.c:34#2)^3- =(uint32) 1;
              (620.c:34#2)^choose {
               -->
                (620.c:34#2)^guard(3-_uint32);
                (620.c:34#2)^goto lbl19;
               -->
                (620.c:34#2)^guard(! 3-_uint32);
              }
            } with lbl19: {
            }
            (620.c:28#2)^3- =(uint32) 3-_uint32;
          }
          (620.c:28#2)^do {
            (620.c:28#2)^while (1) {
              (620.c:28#2)^choose {
               -->
                (620.c:28#2)^guard(2-_uint32);
               -->
                (620.c:28#2)^guard(! 2-_uint32);
                (620.c:28#2)^goto lbl17;
              }
              (620.c:28#2)^2- =(uint32) 2-_uint32;
              (620.c:28#2)^{
                uint32 switch.620.c:28#2.0;
                (620.c:28#2)^choose {
                 -->
                  (620.c:28#2)^guard(! 3-_uint32);
                  (620.c:28#2)^0- =(uint32) 1-_uint32;
                 -->
                  (620.c:28#2)^guard(3-_uint32);
                  (620.c:28#2)^0- =(uint32) 0;
                }
                (620.c:28#2)^do {
                  (620.c:33#2)^do {
                    (620.c:31#2)^do {
                      (620.c:29#2)^do {
                        (620.c:28#2)^choose {
                         -->
                          (620.c:28#2)^guard((0-_uint32 ==_int32 0));
                          (620.c:29#2)^goto lbl28;
                         -->
                          (620.c:28#2)^choose {
                           -->
                            (620.c:28#2)^guard((0-_uint32 ==_int32 1));
                            (620.c:31#2)^goto lbl27;
                           -->
                            (620.c:28#2)^choose {
                             -->
                              (620.c:28#2)^guard((0-_uint32 ==_int32 2));
                              (620.c:33#2)^goto lbl26;
                             -->
                              (620.c:28#2)^guard(! (0-_uint32 ==_int32 2));
                              (620.c:28#2)^guard(! (0-_uint32 ==_int32 1));
                              (620.c:28#2)^guard(! (0-_uint32 ==_int32 0));
                              (620.c:28#2)^goto lbl25;
                            }
                          }
                        }
                      } with lbl28: {
                      }
                      (620.c:30#7)^goto lbl25;
                    } with lbl27: {
                    }
                    (620.c:32#2)^3- =(uint32) 1;
                    (620.c:32#2)^choose {
                     -->
                      (620.c:32#2)^guard(3-_uint32);
                      (620.c:32#2)^goto lbl25;
                     -->
                      (620.c:32#2)^guard(! 3-_uint32);
                    }
                  } with lbl26: {
                  }
                  (620.c:34#2)^3- =(uint32) 1;
                  (620.c:34#2)^choose {
                   -->
                    (620.c:34#2)^guard(3-_uint32);
                    (620.c:34#2)^goto lbl25;
                   -->
                    (620.c:34#2)^guard(! 3-_uint32);
                  }
                } with lbl25: {
                }
                (620.c:28#2)^3- =(uint32) 3-_uint32;
              }
            }
          } with lbl17: {
          }
          (620.c:28#2)^2- =(uint32) 2-_uint32;
        }
      }
    } with lbl1: {
    }
  }
}


