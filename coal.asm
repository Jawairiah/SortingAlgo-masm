TITLE My First Program (Test.asm)
INCLUDE Irvine32.inc

.data
numberofdata DWORD ?        ;number of elements in the array
array DWORD 100 DUP (?)         ;array to store user input values
prompt BYTE "Enter number of elements to sort: ",0
promptInput BYTE "Enter data:", 0
promptOutput BYTE "Sorted output:", 0
menu BYTE "Choose Sorting Algorithm:", 0
menu1 BYTE "1. Bubble Sort", 0
menu2 BYTE "2. Shell Sort", 0
menu3 BYTE "3. Insertion Sort", 0
menu4 BYTE "4. Count Sort", 0
menu5 BYTE "5. Selection Sort", 0
menu6 BYTE "6. Quick Sort",0
menu7 BYTE "7. Comb Sort",0
invalidChoice BYTE "Invalid choice. Exiting program.", 0
reenter byte "Enter 1 to continue sorting ",0
choice DWORD ?              
freqArray DWORD 101 DUP(0)       ;frequency array for CountSort (size = max value + 1)
temp dword ?
key dword ?

.code
main PROC
programloop:
    mov edx, OFFSET prompt
    call WriteString
    call readDec
    mov numberofdata,eax

    mov edx, OFFSET promptInput
    call writestring
    call crlf

    call InputArray
    
    ;DISPLAY MENU
    call crlf
    mov edx, OFFSET menu
    call WriteString
    call crlf
    mov edx, OFFSET menu1
    call WriteString
    call crlf
    mov edx, OFFSET menu2
    call WriteString
    call crlf
    mov edx, OFFSET menu3
    call WriteString
    call crlf
    mov edx, OFFSET menu4
    call WriteString
    call crlf
    mov edx, OFFSET menu5
    call WriteString
    call crlf
    mov edx, OFFSET menu6
    call WriteString
    call crlf
    mov edx, OFFSET menu7
    call WriteString
    call crlf

    call readDec
    mov choice, eax

    ;choosing sort based on user's choice
    cmp choice, 1
    jne choice2
    call BubbleSortArray
    choice2:
    cmp choice, 2
    jne choice3
    call Shellsort
    choice3:
    cmp choice, 3
    jne choice4
    call insertionsort
    choice4:
    cmp choice, 4
    jne choice5
    call CountSort
    choice5:
    cmp choice, 5
    jne choice6
    call SelectionSort
    choice6:
    cmp choice, 7
    jne choice7
    call CombSort
    choice7:
    cmp choice, 6
    jne nexttt
    mov ebx, numberofdata 
    dec ebx
    push ebx
    mov eax, offset array
    push eax
    call QuickSort
    pop eax
    pop ebx
    mov edx, OFFSET reenter
    call WriteString
    call readdec
    cmp eax, 1
    je programloop
    jne exitt

    nexttt:
    
    ;Invalid choice
    ;mov edx, OFFSET invalidChoice
    ;call WriteString
    mov edx, OFFSET reenter
    call WriteString
    call readdec
    cmp eax, 1
    je programloop
    jne exitt

exitt:
ret
main ENDP

;procedure to input elements into the array
InputArray PROC uses ecx esi
    mov ecx, numberofdata     
    mov esi, 0                

InputLoop:
    call readdec               
    mov array[esi], eax        
    add esi, TYPE array        
    loop InputLoop             ;repeat for each element
    ret
InputArray ENDP

; Procedure to print elements of the array
PrintArray PROC uses ecx eax esi
    mov ecx, numberofdata      ;setting loop counter to number of elements
    mov esi, 0              
    mov edx, OFFSET promptOutput
    call writestring
    call crlf

PrintLoop:
    mov eax, array[esi]        
    call writedec              
    call crlf                  
    add esi, TYPE array       
    loop PrintLoop             
    ret
PrintArray ENDP


;//BUBBLESORT
BubbleSortArray PROC uses ecx edx eax ebx
    mov ecx, numberofdata 
    dec ecx
L1:
   mov edx, ecx
   mov esi, 0
   L2:
      mov ebx,array[esi]
      mov eax, array[esi+type  array]
      cmp eax, ebx
      JL swapping
      back:
      add esi,4
      loop L2
    mov ecx, edx
    loop L1
jmp quitt
   
swapping:
mov eax, array[esi]
xchg  eax, array[esi+type  array]
mov array[esi], eax 
mov eax , 5
cmp eax, 1                 
jne back
quitt:
call PrintArray
    ret
BubbleSortArray ENDP


;//SHELLSORT
Shellsort proc uses ecx edx eax ebx
    mov eax, numberofdata
    shr eax, 1                              ; gap = size / 2
    mov ebx, eax                            ; store gap in EBX for the outer loop

OuterLoop:
    cmp ebx, 0                              ; while gap > 0
    jle EndSort                             ; exit if gap <= 0

    ; Inner loop starts: for (j = gap; j < size; j++)
    mov ecx, ebx                            ; j = gap
InnerLoop:
    cmp ecx, numberofdata                   ; if j >= size, break inner loop
    jge EndInnerLoop

    ; temp = array[j]
    mov esi, OFFSET array                   ; array base address
    mov eax, [esi + ecx * type array]       ; temp = array[j]
    
    ; res = j
    mov edi, ecx                            ; res = j

WhileLoop:
    ; while (res >= gap && array[res - gap] > temp)
    cmp edi, ebx                            ; if res < gap, exit while loop
    jl EndWhileLoop

    ; Check if array[res - gap] > temp
    mov edx, edi                            ; calculate (res - gap) * 4
    sub edx, ebx                            ; edx = res - gap
    mov edx, [esi + edx * type array]       ; load array[res - gap] into edx
    cmp edx, eax                            ; compare array[res - gap] with temp
    jle EndWhileLoop

    ; array[res] = array[res - gap]
    mov [esi + edi * type array], edx       ; array[res] = array[res - gap]
    
    ; res -= gap
    sub edi, ebx                            ; res -= gap
    jmp WhileLoop                           ; repeat while loop

EndWhileLoop:
    ; array[res] = temp
    mov [esi + edi * type array], eax                ; array[res] = temp

    ; Increment j in the inner loop
    inc ecx
    jmp InnerLoop

EndInnerLoop:
    ; gap /= 2
    shr ebx, 1                              ; gap = gap / 2
    jmp OuterLoop
EndSort:
call PrintArray
ret
Shellsort endp

;INSERTION SORT ALGORITHM
insertionsort PROC
mov ecx, numberofdata
mov esi, OFFSET array  
mov eax, 1        ;i=1

forLoop:
cmp eax,ecx            ;checking if i<numberofdata
jge FinSort

mov edx, eax           ;temporarily storing i for j
mov edi, edx           ;j=i
mov ebx, [esi+edx*type array]   ;key=array[i]
mov key,ebx

whileLoop:
cmp edi,0 ;j>0
jle assignKey ;if j<=0 exit loop and assign key

mov edx,edi  ;for calculating j-1
dec edx
mov ebx, [esi+edx*type array]  ;arr[j-1]

cmp ebx,key      ;cmp arr[j-1] and key
jle assignKey    ;arr[j-1]<=key

mov [esi+ edi * 4],ebx  ;arr[j]=arr[j-1]
dec edi     ;j--
jmp whileLoop

assignKey:
mov ebx,key              ;loading key into ebx
mov [esi+edi * 4],ebx    ;arr[j]=key
inc eax                  ;i++
jmp forLoop

FinSort:
call PrintArray
ret 
insertionsort ENDP


;Count Sort Procedure
CountSort PROC uses ecx edx eax esi edi
    ;Step 1: Find the max element
    mov eax, array[0]              ; Assume first element is the max
    mov ecx, numberofdata
    lea esi, array                 ; Load base address of array into ESI

FindMax:
    mov edx, [esi]                 ; Load array element
    cmp edx, eax
    jle SkipMax
    mov eax, edx                   ; Update max if found
SkipMax:
    add esi, TYPE array            ; Move to the next element
    loop FindMax

    ; Step 2: Build the frequency array
    lea edi, freqArray             ; Load address of freqArray
    mov ecx, numberofdata          ; Loop through the original array
    lea esi, array
    xor edx, edx                   ; Clear index

BuildFrequency:
    mov eax, [esi]                 ; Load current array element
    inc DWORD PTR [edi + eax * type array]  ; Increment its frequency count
    add esi, TYPE array            ; Move to the next element
    loop BuildFrequency

    ; Step 3: Rebuild the array
    lea esi, array                 ; Load base address of array
    xor eax, eax                   ; Index in freqArray
RebuildArray:
    mov edx, [edi + eax * type array]       ; Frequency of the current value
    test edx, edx
    jz SkipRebuild
WriteElements:
    mov [esi], eax                 ; Write the value
    add esi, TYPE array
    dec edx
    jnz WriteElements
SkipRebuild:
    inc eax
    cmp eax, 101                   ; Check if max value is reached
    jl RebuildArray

    call PrintArray
    ret
CountSort ENDP


; Procedure to sort the array using Selection Sort algorithm
SelectionSort PROC uses ecx esi edi eax ebx
    mov ecx, numberofdata        ; Outer loop: Iterate through each element
    dec ecx                      ; Last element doesn't need comparison

OuterLoop:
    mov esi, ecx                 ; Set current index as the minimum index
    mov edi, esi
    mov ebx, array[edi * type array ]      ; Assume current element as the smallest

InnerLoop:
    cmp edi, 0                   ; Inner loop: Search for the smallest element
    jl EndInnerLoop

    mov eax, array[edi * type array ]      ; Load element at index edi
    cmp eax, ebx                 ; Compare with the current minimum
    jl SkipUpdate
    mov esi, edi                 ; Update minimum index if smaller element found
    mov ebx, eax                 ; Update the smallest value
SkipUpdate:
    dec edi
    jmp InnerLoop

EndInnerLoop:
    cmp esi, ecx                 ; Swap if a smaller element is found
    je SkipSwap                  ; Skip if no smaller element found
    mov eax, array[ecx * type array]      ; Swap current element with the smallest
    mov array[esi * type array], eax
    mov array[ecx * type array], ebx

SkipSwap:
    loop OuterLoop
    call PrintArray
    ret
SelectionSort ENDP



CombSort PROC uses ecx edx eax ebx esi edi
    mov ecx, numberofdata      ; ECX = size of the array
    mov eax, ecx              
    shr eax, 1                 ; Initial gap = size / 2
    mov ebx, eax               ; Store gap in EBX
    mov esi, OFFSET array      ; Base address of the array

CombOuterLoop:
    cmp ebx, 1                 ; If gap <= 1, exit outer loop
    jle CombDone

    mov edi, 0                 ; Initialize index i = 0
CombInnerLoop:
    ; Calculate effective offset for i
    mov eax, edi
    imul eax, type array                ; eax = i * 4 (size of each element in the array)

    ; Check if j = i + gap is out of bounds
    mov edx, ebx
    add edx, edi
    imul edx, type array                ; edx = (i + gap) * 4
    cmp edx, ecx               ; If (i + gap) >= size, break the loop
    jge EndCombInnerLoop

    ; Compare array[i] and array[i + gap]
    mov eax, [esi + edi * 4]   ; eax = array[i]
    mov ebx, [esi + edx]       ; ebx = array[i + gap]
    cmp eax, ebx
    jle SkipSwap

    ; Swap array[i] and array[i + gap]
    mov [esi + edi * type array], ebx   ; array[i] = array[i + gap]
    mov [esi + edx], eax       ; array[i + gap] = eax


SkipSwap:
    inc edi                    ; Move to the next index
    jmp CombInnerLoop          ; Continue inner loop


EndCombInnerLoop:
    ; Reduce the gap by multiplying it by a shrink factor (e.g., 1.3)
    mov eax, ebx               ; Get current gap
    mov edx, 0                 ; Clear high bits for division
    mov ecx, 130               ; Divisor = 1.3 scaled by 100
    imul eax, 100              ; Scale gap by 100
    div ecx                    ; Divide by 130
    test eax, eax              ; Check if gap is zero
    jnz ValidGap
    mov eax, 1                 ; Set gap to 1 if zero

ValidGap:
    mov ebx, eax               ; Update gap
    jmp CombOuterLoop          ; Repeat outer loop

CombDone:
    call PrintArray
    ret
CombSort ENDP







QuickSort PROC
    ;push base pointer onto the stack
    push EBP
    mov EBP, ESP
    push EBX
    push ESI
    push EDI
   
    mov ESI, [EBP+8]    ;ESI is the array
   
    mov EAX, [EBP+12]
    mov ECX, type array
    mul ECX
    mov ECX, EAX
   
    mov EAX, 0
    mov EBX, ECX
   
    call SortMain
       
    ;Restoring the registers
    pop EDI
    pop ESI
    pop EBX
    pop EBP

    call PrintArray
    RET
QuickSort ENDP



;Recursive
SortMain PROC
RecursiveSort:
   
    ;if lowIndex >= highIndex
    cmp EAX, EBX
    jge ExitCondition
   
    push EAX    ;EAX is 'i'
    push EBX    ;EBX is 'j'
    add EBX, type array 
   
    ;EDI is our pivot
    mov EDI, [ESI+EAX]
   
    SortingLoop:

    IncreaseIndexLoop:
        add EAX, type array
        cmp EAX, EBX
        jge EndIncreaseLoop
       
        cmp [ESI+EAX], EDI
        jge EndIncreaseLoop
        jmp IncreaseIndexLoop

    EndIncreaseLoop:
    DecreaseIndexLoop:
        sub EBX, 4
        cmp [ESI+EBX], EDI
    jle EndDecreaseLoop
    jmp DecreaseIndexLoop

    EndDecreaseLoop:
       
        cmp EAX, EBX
    jge EndSortingLoop
       
    push [ESI+EAX]
    push [ESI+EBX]
       
    pop [ESI+EAX]
    pop [ESI+EBX]
       
    jmp SortingLoop
       
    EndSortingLoop:       
   
    pop EDI
    pop ECX
   
    ;If low index == j
    cmp ECX, EBX
    je SkipSwap
       
    ;Else, swap 
    push [ESI+ECX]
    push [ESI+EBX]
       
    pop [ESI+ECX]
    pop [ESI+EBX]
       
    SkipSwap:

    mov EAX, ECX
   
    push EDI    ;Saving the high Index
    push EBX  
   
    sub EBX, type array 
   
    call RecursiveSort
   
    pop EAX
    add EAX, type array 
   
    pop EBX
   
    ;QuickSort(array, j+1, high index)
    call RecursiveSort
   
    ExitCondition:

RET

SortMain ENDP

END main