define hello $45

*= $4500

CODE:
    ADC #$45
    LDA #10
    TAX $5000
    CLD
    CLI
    SBC #57
.END
