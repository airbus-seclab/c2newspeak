// from linux : arch/x86/include/asm/processor.h


typedef struct cpumask { unsigned long bits[(((32) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))]; } cpumask_t;
typedef struct cpumask cpumask_var_t[1];
typedef unsigned char __u8;
typedef unsigned short u16;
typedef unsigned long __u32;

struct cpuinfo_x86 {
 __u8 x86;
 __u8 x86_vendor;
 __u8 x86_model;
 __u8 x86_mask;

 char wp_works_ok;


 char hlt_works_ok;
 char hard_math;
 char rfu;
 char fdiv_bug;
 char f00f_bug;
 char coma_bug;
 char pad0;




 __u8 x86_virt_bits;
 __u8 x86_phys_bits;

 __u8 x86_coreid_bits;

 __u32 extended_cpuid_level;

 int cpuid_level;
 __u32 x86_capability[9];
 char x86_vendor_id[16];
 char x86_model_id[64];

 int x86_cache_size;
 int x86_cache_alignment;
 int x86_power;
 unsigned long loops_per_jiffy;


 cpumask_var_t llc_shared_map;


 u16 x86_max_cores;
 u16 apicid;
 u16 initial_apicid;
 u16 x86_clflush_size;


 u16 booted_cores;

 u16 phys_proc_id;

 u16 cpu_core_id;

 u16 cpu_index;

} __attribute__((__aligned__((1 << (6)))));
extern __attribute__((section(".data..percpu" "..shared_aligned"))) __typeof__(struct cpuinfo_x86) cpu_info __attribute__((__aligned__((1 << (6)))));
extern unsigned long __per_cpu_offset[32];

static inline int hlt_works(int cpu)
{

 return (*({ do { const void *__vpp_verify = (typeof((&(cpu_info))))((void *)0); (void)__vpp_verify; } while (0); ({ unsigned long __ptr; __asm__ ("" : "=r"(__ptr) : "0"((typeof(*(&(cpu_info))) *)(&(cpu_info)))); (typeof((typeof(*(&(cpu_info))) *)(&(cpu_info)))) (__ptr + (((__per_cpu_offset[cpu])))); }); })).hlt_works_ok;



}
