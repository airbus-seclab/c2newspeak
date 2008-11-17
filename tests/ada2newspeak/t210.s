	.file	"t210.adb"
	.comm	_t210_E, 16	 # 1
	.text
.globl _t210__f
	.def	_t210__f;	.scl	2;	.type	32;	.endef
_t210__f:
	pushl	%ebp
	movl	%esp, %ebp
	movl	8(%ebp), %eax
	incl	%eax
	popl	%ebp
	ret
	.def	_t210__appelfonction;	.scl	3;	.type	32;	.endef
_t210__appelfonction:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	$2, (%esp)
	call	_t210__f
	movl	%eax, -4(%ebp)
	leave
	ret
