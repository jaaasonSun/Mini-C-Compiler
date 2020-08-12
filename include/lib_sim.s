        .global putint
        .global getint
        .global putchar

getint:
        li a0, 1
        ecall
        mv a0, a1
        ret

putint:
        mv a1, a0
        li a0, 2
        ecall
        ret

putchar:
        mv a1, a0
        li a0, 3
        ecall
        ret
