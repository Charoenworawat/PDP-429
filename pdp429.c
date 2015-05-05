/*
  PDP-429 Interpreter:

   opcodes include:

   mem-ref:  TAD, AND, ISZ, DCA, JMS, JMP
   operate:  CLA, CLL, CMA, CML, RAR, RAL, IAC, SMA, SZA, SNL, OSR, HLT,
             NOP, SPA, SNA, SZL, SKP  RTR, RTL
   i/o:      IOT

*/

/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

FILE *input;

typedef short Boolean;
#define TRUE 1
#define FALSE 0

Boolean debug = FALSE;
Boolean verbose = FALSE;
Boolean ASCII = FALSE;

typedef char *STRING;

#define CAST(t,e) ((t)(e))
#define TYPED_MALLOC(t) CAST(t*, malloc(sizeof(t)))


/* ***************************************************************** */
/*                                                                   */
/* print representation of a character for debugging                 */
/*                                                                   */
char   *printrep (unsigned short  c)
{
    static char pr[8];

    if (c < 32)
        {
            /* control characters */
            pr[0] = '^';
            pr[1] = c + 64;
            pr[2] = '\0';
        }
    else if (c < 127)
        {
            /* printing characters */
            pr[0] = c;
            pr[1] = '\0';
        }
    else if (c == 127)
        return("<del>");
    else if (c <= 0377)
        {
            /* upper 128 codes from 128 to 255;  print as \ooo - octal  */
            pr[0] = '\\';
            pr[3] = '0' + (c & 7);
            c = c >> 3;
            pr[2] = '0' + (c & 7);
            c = c >> 3;
            pr[1] = '0' + (c & 3);
            pr[4] = '\0';
        }
    else
        {
            /* very large number -- print as 0xffff - 4 digit hex */
            (void)sprintf(pr, "0x%04x", c);
        }
    return(pr);
}


/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

/* MALLOC space for a string and copy it */

STRING remember_string(const STRING name)
{
    size_t n;
    STRING p;

    if (name == NULL) return(NULL);

    /* get memory to remember file name */
    n = strlen(name) + 1;
    p = CAST(STRING, malloc(n));
    strcpy(p, name);
    return(p);
}


/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

char line[10];
int line_length = 8;

int get_next_line(void)
{
    /* get the first character to see if we have EOF */
    int c;
    int i = 0;
    c = getc(input);
    if (c != EOF)
        {
            line[i] = c;
            i = 1;
            while (((c = getc(input)) != EOF) && (c != '\n'))
                {
                    if (i < line_length)
                        {
                            line[i] = c;
                            i = i + 1;
                        }
                }
        }
    line[i] = '\0';

    if (debug) fprintf(stderr, "next input line: %s\n", line);

    if ((c == EOF) && (i == 0))
        return(EOF);
    else
        return(i);
}



/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

Boolean is_hex(char c)
{
    if (('0' <= c) && (c <= '9')) return(TRUE);
    if (('A' <= c) && (c <= 'F')) return(TRUE);
    if (('a' <= c) && (c <= 'f')) return(TRUE);
    return(FALSE);
}

int hex_char_value(char c)
{
    if (('0' <= c) && (c <= '9')) return(c-'0');
    if (('A' <= c) && (c <= 'F')) return(c-'A' + 10);
    if (('a' <= c) && (c <= 'f')) return(c-'a' + 10);
    return(-1);
}

int hex_value(char *p)
{
    int n = 0;
    while (is_hex(*p))
        {
            n = n * 16 + hex_char_value(*p);
            p++;
        }
    return(n);
}


/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

/* PDP-429 types */

typedef unsigned short Address;
typedef unsigned short INST;
typedef unsigned short Word;


#define MASK_W16      0xFFFF // 16 bits
#define MASK_UPPER8   0xFF00
#define MASK_LOWER8   0x00FF
#define MASK_SIGN_BIT 0x8000

#define MASK_REG_SPEC 0x0C00

#define MASK_DI_BIT  0x0200
#define MASK_ZC_BIT  0x0100
#define MASK_OFFSET  0x00FF

// No Overflow due to full utilization of 16 bits
#define MASK_OVERFLOW 0xFFFF0000    /* Any bit in higher order than the 16 bits is overflow */

// Non-Register Memory Reference Instruction Masks
#define MASK_SMx 0x0200
#define MASK_SZx 0x0100
#define MASK_SNL 0x0080
#define MASK_RSS 0x0040
#define MASK_CLx 0x0020
#define MASK_CLL 0x0010
#define MASK_CMx 0x0008
#define MASK_CML 0x0004
#define MASK_DCx 0x0002
#define MASK_INx 0x0001

#define MASK_IOT_DEVICE   0x03F8
#define MASK_IOT_FUNCTION 0x0007


/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

INST     memory[65536];
Boolean defined[65536];

/* registers */
//Address PC = 0; // R_Index of 4 (R[4])

int R_Index = 0;
Word R[8]; // A B C D PC PSW SP SPL Registers

char* R_Names[8] = {"A", "B", "C", "D", "PC", "PSW", "SP", "SPL"};

// Link 'Bit'
Word L = 0;


/* internal controls */
Word Switch_Register = 0;
Boolean Halted = TRUE;
Boolean stack_error = FALSE;
long long time = 0;


/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */


void Clear_Memory(void)
{
    int i;
    for (i = 0; i < 65536; i++)
        {
            defined[i] = FALSE;
        }
}

void Store_Memory(Address addr, Word value)
{
    if (debug)
        fprintf(stderr, "write memory: 0x%03X = 0x%03X\n", addr, value);
    defined[addr] = TRUE;
    memory[addr] = value & MASK_W16;

    time = time + 1;
}

INST Fetch_Memory(Address addr)
{
    Word value;

    if (defined[addr])
        value = memory[addr];
    else
        value = 0;

    time = time + 1;

    if (debug)
        fprintf(stderr, "read memory: 0x%03X = 0x%03X\n", addr, value);
    return(value);
}

/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

Address Load_ASCII_Object_File(STRING name)
{
    Address entry_point = 0;
    Word data;

    while (get_next_line() != EOF)
        {
            char *p = line;
            while (isalnum(*p)) p++;
            *p = '\0';
            while (!is_hex(*p)) p++;
            /* two values: one at line, the other at p */
            data = hex_value(p);
            if (strcasecmp(line, "EP") == 0)
                entry_point = data;
            else
                {
                    Address addr = hex_value(line);
                    Store_Memory(addr, data);
                }
        }

    return(entry_point);
}

/* ***************************************************************** */
/*                                                                   */
/* ***************************************************************** */

int get2(void)
{
    int c1 = getc(input);
    int c2 = getc(input);
    if (debug) fprintf(stderr, "read two bytes: 0x%X, 0x%X\n", c1, c2);
    if ((c1 == EOF) || (c2 == EOF))
        {
            fprintf(stderr, "Premature EOF\n");
            exit(1);
        }
    if (c1 & (~0xFF)) fprintf(stderr, "Extra high order bits for 0x%X\n", c1);
    if (c2 & (~0xFF)) fprintf(stderr, "Extra high order bits for 0x%X\n", c2);
    int n = ((c1 & 0xFF) << 8) | (c2 & 0xFF);
    return(n);
}

Address Load_Binary_Object_File(STRING name)
{
    int c1 = getc(input);
    int c2 = getc(input);
    int c3 = getc(input);
    int c4 = getc(input);
    if (debug) fprintf(stderr, "read four bytes: 0x%X, 0x%X, 0x%X, 0x%X\n", c1, c2, c3, c4);

    if ((c1 != 'O') || (c2 != 'B') || (c3 != 'J') || (c4 != 'G'))
        {
            fprintf(stdout, "First four bytes are not OBJG: ");
            fprintf(stdout, "%s", printrep(c1));
            fprintf(stdout, "%s", printrep(c2));
            fprintf(stdout, "%s", printrep(c3));
            fprintf(stdout, "%s", printrep(c4));
            fprintf(stdout, " (%02X %02X %02X %02X)\n", c1, c2, c3, c4);

            exit(1);
        }

    Address entry_point = get2();

    int n;
    while ((n = getc(input)) != EOF)
        {
            if (debug) fprintf(stderr, "Read next block of %d bytes\n", n);
            n = n - 1;
            Address addr = get2(); n -= 2;
            while (n > 0)
                {
                    Word data = get2(); n -= 2;            
                    Store_Memory(addr, data);
                    addr += 1;
                }
        }

    return(entry_point);
}

void Load_Object_File(STRING name)
{
    Address entry_point = 0;

    Clear_Memory();

    if (ASCII)
        entry_point = Load_ASCII_Object_File(name);
    else
        entry_point = Load_Binary_Object_File(name);

    time = 0;
    Halted = FALSE;
    R[4] = entry_point & MASK_W16;
}


/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

/* constructing the opcode name for an instruction */

char opcode_name[64];

void clear_opcode(void)
{
    opcode_name[0] = '\0';
}

void append_opcode(STRING name)
{
    if (opcode_name[0] != '\0')
        strncat(opcode_name, " ", sizeof(opcode_name));
    strncat(opcode_name, name, sizeof(opcode_name));
}

char *get_opcode(void)
{
    return(opcode_name);
}


// Register Activity
char register_activity[256];

void clear_register_activity(void)
{
    register_activity[0] = '\0';
}

void append_register_activity(STRING activity)
{
    strcpy(register_activity, activity);
}

char* get_register_activity(void)
{
    return(register_activity);
}

char* toString(int x)
{
    char* tmp = malloc(8);
    sprintf(tmp, "0x%04X", x);
    return tmp;
}


/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

void Check_Overflow(void)
{
    /* check for overflow and complement L if so */
    if ((R[R_Index] & MASK_OVERFLOW) != 0) L = 1-L;
    R[R_Index] = R[R_Index] & MASK_W16;
}

int Decode_Instruction(INST inst)
{
    return((inst >> 12) & 0xF);
}

void Set_R_Index(INST inst)
{
    R_Index = ((inst >> 10) & 0x3); 
}

void Stack_Push(Word value)
{
    // SP    SPL
    if(R[6] < R[7]) // Stack Overflow
	{
    	    stack_error = TRUE;
	    fprintf(stderr, "Stack Pointer = 0x%04X; Stack Limit = 0x%04X\n", R[6], R[7]);
	}

    Store_Memory(R[6], value);
    R[6] = R[6]-1;
}
Word Stack_Pop(void)
{
    // SP
    if(R[6] == 0xFFFF) // Stack Underflow
	{
    	    stack_error = TRUE;
	    fprintf(stderr, "Stack Pointer = 0xFFFF\n");
	}	

    R[6] = R[6]+1;
    return Fetch_Memory(R[6]);
}

void NonRegister_Memory(Address old_PC, int opcode, INST inst)
{
    char ra[256];
    strcpy(ra, "");

    // get the new addr
    Address addr = inst & MASK_OFFSET;
    // check for Z/C = 1 -> current page 
    if ((inst & MASK_ZC_BIT) != 0)
        addr = addr | (old_PC & ~MASK_OFFSET);
    // check for I/D = 1 -> indirect 
    if ((inst & MASK_DI_BIT) != 0)
        {
            append_opcode("I");
	    strcat(ra, "M[");
	    strcat(ra, toString(addr));
	    strcat(ra, "] -> ");

            addr = Fetch_Memory(addr);

	    strcat(ra, toString(addr));
	    strcat(ra, ", ");

            //time = time + 1;
        }

    Word value;
    int sub_opcode = (inst & 0x0C00) >> 10;

    if(opcode == 11 && sub_opcode == 0)
	{
	    value = Fetch_Memory(addr)+1;
	    Store_Memory(addr, value);

	    append_opcode("ISZ");

	    strcat(ra, "M[");
	    strcat(ra, toString(addr));
	    strcat(ra, "] -> ");
	    strcat(ra, toString(value));
	    strcat(ra, ", ");


	    strcat(ra, toString(value));
	    strcat(ra, " -> ");
	    strcat(ra, "M[");
	    strcat(ra, toString(addr));
	    strcat(ra, "]");

	    if(value == 0)
		R[4] = R[4]+1;
	}
    else if(opcode == 11 && sub_opcode == 1)
	{
	    append_opcode("JMP");

	    strcat(ra, toString(addr));
	    strcat(ra, " -> PC");

	    R[4] = addr; 
	}
    else if(opcode == 11 && sub_opcode == 2)
	{
	    append_opcode("CALL");

	    strcat(ra, toString(R[4]));
	    strcat(ra, " -> M["); 
	    strcat(ra, toString(R[6]));
	    strcat(ra, "], ");

	    // push PC+1 onto stack
	    Stack_Push(R[4]);

	    // SP adjusted in Stack_Push()
	    strcat(ra, toString(R[6]));
	    strcat(ra, " -> SP, ");

	    // address of memory-operand -> PC
	    R[4] = addr;
	    //time = time + 1; // storing a value

	    strcat(ra, toString(addr));
	    strcat(ra, " -> PC");
	}
    else if(opcode == 12 && sub_opcode == 0)
	{
	    append_opcode("PUSH");

	    // push memory-operand to the stack
	    value = Fetch_Memory(addr);
	    Stack_Push(value);

	    strcat(ra, "M[");
	    strcat(ra, toString(addr));
	    strcat(ra, "] -> ");
	    strcat(ra, toString(value));
	    strcat(ra, ", ");
	    
	    if(stack_error)
		{
	    	    append_opcode("Stack Overflow");

		    
	    	    strcat(ra, "PSW -> ");
	    	    strcat(ra, toString(R[5]));
	    	    strcat(ra, ", ");
	
	    	    R[5] = 0;
	    	    Halted = TRUE;

	    	    strcat(ra, toString(R[5]));
	    	    strcat(ra, " -> PSW");

	 	    time = time - 1;
		}
	    else
		{
	    	    strcat(ra, toString(value));
	    	    strcat(ra, " -> M[");
	    	    strcat(ra, toString(addr));
	    	    strcat(ra, ", ");

	    	    strcat(ra, toString(R[6]));
	    	    strcat(ra, " -> SP");
		}

	}
    else if(opcode == 12 && sub_opcode == 1)
	{
	    append_opcode("POP");

	    // pop top of stack and store in memory-operand
	    strcat(ra, "SP -> ");  	// SP Address
	    strcat(ra, toString(R[6]));
	    strcat(ra, ", ");

	    value = Stack_Pop();
	    memory[R[6]] = value;
	    Store_Memory(addr, value);

	    strcat(ra, "M[");	 	 // SP Address+1 -> SP Value
	    strcat(ra, toString(R[6]));
	    strcat(ra, "] -> ");
	    strcat(ra, toString(value)); 
	    strcat(ra, ", ");

	    strcat(ra, toString(value)); // SP Value -> Memory-Operand
	    strcat(ra, " -> M[");
	    strcat(ra, toString(addr));
	    strcat(ra, "]");

	    if(stack_error)
		{
		    ra[0] = '\0';
		    strcpy(ra, "");
	    	    strcat(ra, "M[");
	    	    strcat(ra, toString(addr));
	    	    strcat(ra, "] -> ");
	    	    strcat(ra, toString(value));
	    	    strcat(ra, ", ");

	    	    append_opcode("Stack Underflow");

	    	    strcat(ra, "PSW -> ");
	    	    strcat(ra, toString(R[5]));
	    	    strcat(ra, ", ");
	
	    	    R[5] = 0;
	    	    Halted = TRUE;

	    	    strcat(ra, toString(R[5]));
	    	    strcat(ra, " -> PSW");
		}
	}

    append_register_activity(ra);
}

void Register_Memory(Address old_PC, int opcode, INST inst)
{
    char op[80]; 
    char ra[256];
    strcpy(ra, "");

    // get the new addr
    Address addr = inst & MASK_OFFSET;
    // check for Z/C = 1 -> current page 
    if ((inst & MASK_ZC_BIT) != 0)
        addr = addr | (old_PC & ~MASK_OFFSET);
    // check for I/D = 1 -> indirect 
    if ((inst & MASK_DI_BIT) != 0)
        {
            append_opcode("I");
	    strcat(ra, "M[");
	    strcat(ra, toString(addr));
	    strcat(ra, "] -> ");

            addr = Fetch_Memory(addr);

	    strcat(ra, toString(addr));
	    strcat(ra, ", ");

            //time = time + 1;
        }


    int Memory_Operand = Fetch_Memory(addr);

    if(opcode != 8)
    {
	    // Register Data
	    strcat(ra, R_Names[R_Index]);
	    strcat(ra, " -> ");
	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, ", ");

	if(opcode != 9)
	{
	    // Memory-Operand Data
	    strcat(ra, "M[");
	    strcat(ra, toString(addr));
	    strcat(ra, "] -> ");
	    strcat(ra, toString(Memory_Operand));
	    strcat(ra, ", ");
	}
    }

    switch(opcode)
	{
	case 1: // ADD*
	    strcpy(op, "ADD");

	    R[R_Index] = R[R_Index] + Memory_Operand;
	    Check_Overflow();

	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, " -> ");
	    strcat(ra, R_Names[R_Index]);
	    break;

	case 2: // SUB*
	    strcpy(op, "SUB");

	    R[R_Index] = R[R_Index] - Memory_Operand;

	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, " -> ");
	    strcat(ra, R_Names[R_Index]);
	    break;

	case 3: // MUL*
	    strcpy(op, "MUL");

	    R[R_Index] = R[R_Index] * Memory_Operand;
	    Check_Overflow();

	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, " -> ");
	    strcat(ra, R_Names[R_Index]);
	    break;

	case 4: // DIV*
	    strcpy(op, "DIV");
	    R[R_Index] = R[R_Index] / Memory_Operand;
fprintf(stderr, "YO\n");

	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, " -> ");
	    strcat(ra, R_Names[R_Index]);
	    break;

	case 5: // AND*
	    strcpy(op, "AND");

	    R[R_Index] = R[R_Index] & Memory_Operand;

	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, " -> ");
	    strcat(ra, R_Names[R_Index]);
	    break;

	case 6: // OR*
	    strcpy(op, "OR");

	    R[R_Index] = R[R_Index] | Memory_Operand;

	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, " -> ");
	    strcat(ra, R_Names[R_Index]);
	    break;

	case 7: // XOR*
	    strcpy(op, "XOR");

	    R[R_Index] = R[R_Index] ^ Memory_Operand;

	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, " -> ");
	    strcat(ra, R_Names[R_Index]);
	    break;

	case 8: // LD*: memory-operand -> Reg
	    strcpy(op, "LD");

	    strcat(ra, "M[");
	    strcat(ra, toString(addr));
	    strcat(ra, "] -> ");
	    strcat(ra, toString(Memory_Operand));
	    strcat(ra, ", ");
	    strcat(ra, toString(Memory_Operand));
	    strcat(ra, " -> ");
	    strcat(ra, R_Names[R_Index]);

	    R[R_Index] = Memory_Operand;
	    break;

	case 9: // ST*
	    strcpy(op, "ST");

	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, " -> M[");
	    strcat(ra, toString(addr));
	    strcat(ra, "]");

	    Store_Memory(addr, R[R_Index]);
	    time = time - 1; // already updated time but didnt use memory-operand
	    break;
	}

	if(R_Index == 0)
	    strcat(op, "A");
	else if(R_Index == 1)
	    strcat(op, "B");
	else if(R_Index == 2)
	    strcat(op, "C");
	else if(R_Index == 3)
	    strcat(op, "D");

	append_opcode(op);
	append_register_activity(ra);
}

void NonRegister_NonMemory(INST inst)
{
    char ra[256];
    strcpy(ra, "");

    int value = inst & MASK_W16;
    switch(value)
    {
	case 0: // NOP
            append_opcode("NOP");
	    break;
		
	case 1: // HLT -- The low-order bit of the PSW is set to 0
	    append_opcode("HLT");
	
	    strcat(ra, "PSW -> ");
	    strcat(ra, toString(R[5]));
	    strcat(ra, ", ");
	
	    R[5] = 0;

	    strcat(ra, toString(R[5]));
	    strcat(ra, " -> PSW");

	    Halted = TRUE;
            break;
	
	case 2: // RET -- Pop the stack into the PC
            append_opcode("RET");

	    // pop top of stack and store in memory-operand
	    strcat(ra, "SP -> ");  	// SP Address
	    strcat(ra, toString(R[6]));
	    strcat(ra, ", ");
	
	    Word tmp;
	    tmp = Stack_Pop();
	    R[4] = tmp; // popped data into PC

	    strcat(ra, toString(R[6])); // SP Address +1 -> SP
	    strcat(ra, " -> SP, ");

	    strcat(ra, "M[");
	    strcat(ra, toString(R[6]));
	    strcat(ra, "] -> ");
	    strcat(ra, toString(tmp));
	    strcat(ra, ", ");

	    strcat(ra, toString(tmp));
	    strcat(ra, " -> PC");
	    break;
    }

    append_register_activity(ra);
}

void Register_Register(INST inst)
{
    char ra[128];
    strcpy(ra, "");

    int RegI_Index = (inst & 0x01C0) >> 6;

    int RegJ_Index = (inst & 0x0038) >> 3;
    Word J = R[RegJ_Index];

    int RegK_Index = (inst & 0x0007);
    Word K = R[RegK_Index];

	    strcat(ra, R_Names[RegJ_Index]);
	    strcat(ra, " -> ");
	    strcat(ra, toString(R[RegJ_Index]));
	    strcat(ra, ", ");
	    strcat(ra, R_Names[RegK_Index]);
	    strcat(ra, " -> ");
	    strcat(ra, toString(R[RegK_Index]));
	    strcat(ra, ", ");

    int sub_opcode = (inst & 0x0E00) >> 9;
    switch(sub_opcode)
	{
	case 0: // MOD: Reg[j]%Reg[k] -> Reg[i]
            append_opcode("MOD");

	    R[RegI_Index] =  J % K;    
	    break;

	case 1: // ADD: J + K -> I
            append_opcode("ADD");

	    R[RegI_Index] =  J + K;    
	    Check_Overflow();
	    break;

	case 2: // SUB: J - K -> I
            append_opcode("SUB");

	    R[RegI_Index] =  J - K;    
	    break;

	case 3: // MUL: J * K -> I
            append_opcode("MUL");

	    R[RegI_Index] =  J * K;    
	    Check_Overflow();
	    break;

	case 4: // DIV: J / K -> I
            append_opcode("DIV");

	    R[RegI_Index] =  J / K;    
	    break;

	case 5: // AND: J & K -> I
            append_opcode("AND");

	    R[RegI_Index] =  J & K;    
	    break;

	case 6: // OR: J | K -> I
            append_opcode("OR");

	    R[RegI_Index] =  J | K;    
	    break;

	case 7: // XOR: J ^ K -> I
            append_opcode("XOR");

	    R[RegI_Index] =  J ^ K;    
	    break;
	}

	    strcat(ra, toString(R[RegI_Index])); 
	    strcat(ra, " -> ");
	    strcat(ra, R_Names[RegI_Index]);

	append_register_activity(ra);
}

void NonMemory_Register(INST inst)
{
    char op[80];
    strcpy(op, "");
    char ra[256];
    strcpy(ra, "");
    
    Boolean repeat = FALSE;
    Boolean skip = FALSE;

    /* SM* */
    if (inst & MASK_SMx)
        {
 	    strcat(op, "SM");
	    strcat(op, R_Names[R_Index]);
	    strcat(op, " ");

	    strcat(ra, R_Names[R_Index]);
	    strcat(ra, " -> ");
	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, ", ");

            skip = skip || ((R[R_Index] & MASK_SIGN_BIT) != 0);
	    repeat = TRUE;
        }

    /* SZ* */
    if (inst & MASK_SZx)
        {
 	    strcat(op, "SZ");
	    strcat(op, R_Names[R_Index]);
	    strcat(op, " ");

	if(!repeat)
	{
	    strcat(ra, R_Names[R_Index]);
	    strcat(ra, " -> ");
	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, ", ");
	}

            skip = skip || (R[R_Index] == 0);
        }

    /* SNL */
    if (inst & MASK_SNL)
        {
            strcat(op, "SNL ");
            skip = skip || (L != 0);
        }

    /* RSS */
    if (inst & MASK_RSS)
        {
            strcat(op, "RSS ");
            skip = !skip;
        }

    if (skip)
	{
 	    R[4] = (R[4] + 1) & MASK_W16;

	    strcat(ra, toString(R[4]));
	    strcat(ra, " -> PC, ");
	}
    

    /* CL* */
    if (inst & MASK_CLx)
        {
 	    strcat(op, "CL");
	    strcat(op, R_Names[R_Index]);
	    strcat(op, " ");

	    strcat(ra, "0x0000 -> ");
	    strcat(ra, R_Names[R_Index]);
	    strcat(ra, ", ");

            R[R_Index] = 0;
        }

    /* CLL */
    if (inst & MASK_CLL)
        {
            strcat(op, "CLL ");

	    strcat(ra, "0x0000 -> L, ");

            L = 0;

	    strcat(ra, "L -> ");
	    strcat(ra, toString(L));
	    strcat(ra, ", ");
        }

    /* CM* */
    if (inst & MASK_CMx)
        {
 	    strcat(op, "CM");
	    strcat(op, R_Names[R_Index]);
	    strcat(op, " ");

	    strcat(ra, R_Names[R_Index]);
	    strcat(ra, " -> ");
	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, ", ");

            R[R_Index] = (~R[R_Index]) & MASK_W16;

	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, " -> "); 
	    strcat(ra, R_Names[R_Index]);
	    strcat(ra, ", ");
        }

    /* CML */
    if (inst & MASK_CML)
        {
            strcat(op, "CML ");

	    strcat(ra, "L -> ");
	    strcat(ra, toString(L));
	    strcat(ra, ", ");

            L = 1-L;

	    strcat(ra, toString(L));
	    strcat(ra, " -> L, ");
        }

    /* IN* */
    if (inst & MASK_INx)
        {
 	    strcat(op, "IN");
	    strcat(op, R_Names[R_Index]);
	    strcat(op, " ");

	    strcat(ra, R_Names[R_Index]);
	    strcat(ra, " -> ");
	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, ", ");

            R[R_Index] = R[R_Index]+1;
            Check_Overflow();  

	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, " -> ");
	    strcat(ra, R_Names[R_Index]);
	    strcat(ra, ", ");
        }

    /* DC* */
    if (inst & MASK_DCx)
        {
 	    strcat(op, "DC");
	    strcat(op, R_Names[R_Index]);
	    strcat(op, " ");

	    strcat(ra, R_Names[R_Index]);
	    strcat(ra, " -> ");
	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, ", ");

            R[R_Index] = R[R_Index]-1;

	    strcat(ra, toString(R[R_Index]));
	    strcat(ra, "-> ");
	    strcat(ra, R_Names[R_Index]);
	    strcat(ra, ", ");
        }

    // Fix Fence-Post Problem
    op[strlen(op)-1] = '\0'; // trailing space
    ra[strlen(ra)-2] = '\0'; // trailing comma and space

    append_opcode(op);
    append_register_activity(ra);
}


void Execute(Address old_PC, int opcode, INST inst)
{
    Address addr;
    Word value;

    /* zero the opcode names */
    clear_opcode();
    clear_register_activity();

    switch(opcode)
        {
        case 0: /* Non-Register Non-Memory Instructions */
	    NonRegister_NonMemory(inst);
            //time = time + 1;
            break;
           
//-----------------Register Memory Reference Instructions-------------------// 

        case 1: 
        case 2: 
        case 3:
        case 4:
        case 5:
        case 6:
        case 7:
        case 8:
        case 9:
	    Set_R_Index(inst);
	    Register_Memory(old_PC, opcode, inst);
            //time = time + 1;
            break;
//-------------------------------------------------------------------------// 

//--------------Non-Register Memory Reference Instructions-----------------// 
	case 11:
	case 12:
	    NonRegister_Memory(old_PC, opcode, inst);
            //time = time + 1;
            break;
//-------------------------------------------------------------------------// 
            
        case 10: /* IOT -- Input/Output Transfer  */
            {
                int device = (inst & MASK_IOT_DEVICE) >> 3;
                int function = (inst & MASK_IOT_FUNCTION);
		Set_R_Index(inst);

                /* check for device = 3 -- Input */
                if (device == 3)
                    {
                        append_opcode("IOT 3");
                        R[R_Index] = getc(stdin) & MASK_W16;
                    }
                /* or device = 4 -- Output */
                else if (device == 4)
                    {
                        append_opcode("IOT 4");
			
	 		char ra[256];
		 	strcpy(ra, R_Names[R_Index]);
			strcat(ra, " -> ");
			strcat(ra, toString(R[R_Index] & 0xFF));
			append_register_activity(ra);

                        putc((R[R_Index] & 0xFF), stdout); 
                    }
                else
                    {
                        append_opcode("IOT <bad-device>");
                        fprintf(stderr, "IOT function %d to unknown device %d; halting\n", function, device);
                        Halted = TRUE;
                    }
            
                //time = time + 1;
                break;
            }

	case 14: // Register-to-Register Instructions
            Register_Register(inst);
	    //time = time + 1;
	    break;
            
        case 15: // Non-Memory Register Instructions
	    Set_R_Index(inst);
            NonMemory_Register(inst);
            //time = time + 1;
            break;
        }

    if (verbose)
    fprintf(stderr, "Time %3lld: PC=0x%04X instruction = 0x%04X (%s)",
		    time, old_PC, inst, get_opcode()); 
    char* regs = get_register_activity();
    if (regs != NULL)
	fprintf(stderr, ": %s", regs);
    fprintf(stderr, "\n");
}


/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

void Interpreter(STRING name)
{
    Load_Object_File(name);
    R[5] = 1; // default PSW

    while (!Halted)
        {
            Address old_PC = R[4];
            INST inst = Fetch_Memory(R[4]);
            R[4] = (R[4] + 1) & MASK_W16;
            int opcode = Decode_Instruction(inst);
            Execute(old_PC, opcode, inst);
//if(time > 1700)
    //exit(0);
        }
}


/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

void scanargs(STRING s)
{
    /* check each character of the option list for
       its meaning. */

    while (*++s != '\0')
        switch (*s)
            {

            case 'D': /* debug option */
                debug = TRUE;

            case 'b': /* binary object file input */
                ASCII = FALSE;
                break;

            case 'a': /* ASCII object file input */
                ASCII = TRUE;
                break;

            case 'v': /* verbose option */
                verbose = !verbose;
                break;

            case 's': /* switch register setting */
            case 'S': /* switch register setting */
                Switch_Register = hex_value(&s[1]) & MASK_W16;
                if (debug) fprintf(stderr, "Switch Register is 0x%03X\n", Switch_Register);
                break;

            default:
                fprintf (stderr,"pdp429: Bad option %c\n", *s);
                fprintf (stderr,"usage: pdp429 [-D] file\n");
                exit(1);
            }
}



/* ***************************************************************** */
/*                                                                   */
/*                                                                   */
/* ***************************************************************** */

int main(int argc, STRING *argv)
{
    Boolean filenamenotgiven = TRUE;

    /* main driver program.  Define the input file
       from either standard input or a name on the
       command line.  Process all arguments. */

    while (argc > 1)
        {
            argc--, argv++;
            if (**argv == '-')
                scanargs(*argv);
            else
                {
                    filenamenotgiven = FALSE;
                    input = fopen(*argv,"r");
                    if (input == NULL)
                        {
                            fprintf (stderr, "Can't open %s\n",*argv);
                        }
                    else
                        {
                            Interpreter(*argv);
                            fclose(input);
                        }
                }
        }

    if (filenamenotgiven)
        {
            input = stdin;
            Interpreter(NULL);
        }

    exit(0);
}


