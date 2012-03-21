struct gdt_page {
     int gdt[32];
} __attribute__((aligned(((1UL) << 12))));
