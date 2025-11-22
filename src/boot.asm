; stage 1

; 16-bit real-mode
[bits 16]

; BIOS always loads us to this location
[org 0x7c00]

; export a map of boot loader symbols for debugging	
[map symbols boot.map]

stage1:

; no interrupts, initializing segment registers and a real mode stack
; growing downwards from 0x7c00 where the stage 1 code and data lives
	cli
	cld
	xor ax, ax
	mov ds, ax
	mov ss, ax
	mov sp, 0x7c00
	mov bp, sp
	sti

; dl contains the boot drive, primarily because that's the last function
; called by the BIOS MBR loader, we remember that for loading additional
; blocks from the boot medium
	mov [BOOT_DRIVE], dl

; print greeting message
	mov si, MESSAGE_GREETING
	call print_string

; size of stage 2 in sectors (3584-512 bytes, size of boot.img minus stage 1)
NOF_SECTORS_STAGE2 equ 6

; load stage 2 with simple load method to 0x7e00 (directly
; after the boot sector), we assume stage 2 fits on a track
	mov ah, 0x02			; read sectors from drive
	mov al, NOF_SECTORS_STAGE2	; read sectors of stage 2
	mov ch, 0			; select first cylinder
	mov dl, [BOOT_DRIVE]		; drive to read from
	mov dh, 0			; first head
	mov cl, 2			; second sector after boot sector
	mov bx, 0			; where to store the data
	mov es, bx
	mov bx, 0x7e00			; 512 bytes after first sector		
	int 0x13
	jc .read_error
	jmp .read_ok
	
.read_error:
	mov di, ERR_DISK
	mov ax, 0x01
	call print_error
.read_ok:
	cmp al, NOF_SECTORS_STAGE2	; correct number of sectors read?
	jne .short_read_error		; if not, short read
	jmp .short_read_ok
.short_read_error:
	mov di, ERR_DISK
	mov ax, 0x02
	call print_error
.short_read_ok:
	jmp stage2

; di: pointer to additional message
; ax: error code in ASCII, never returns
print_error:
	mov si, NL
	call print_string
	mov si, MESSAGE_ERROR
	call print_string
	mov si, di
	call print_string
	call print_hex_nice
	call kill_motor
	mov si, NL
	call print_string
	jmp reboot

; IN si
print_string:
	push ax
	push bx
.loop:
	lodsb
	cmp al, 0
	je .fini
	call print_char
	jmp .loop
.fini:
	pop bx
	pop ax
	ret

; IN al: character to print
; MOD ah
print_char:
	mov ah, 0x0e
	int 0x10
	ret

; IN ah: hex value to print	
print_hex_nice:
	push si
	mov si, HEX_PREFIX
	call print_string
	call print_hex
	mov si, SPACE
	call print_string	
	pop si
	ret

; IN ah: hex value to print	
print_hex:
	push bx
	push si
	mov si, HEX_TEMPLATE
	mov bx, ax
	and bx, 0x00FF
	shr bx, 4
	mov bx, [HEXABET+bx]
	mov [HEX_TEMPLATE], bl
	mov bx, ax
	and bx, 0x000F
	mov bx, [HEXABET+bx]
	mov [HEX_TEMPLATE+1], bl
	call print_string
	pop si
	pop bx
	ret

; IN: eax: value to print in hex
print_hex_dword:
	push eax
	push ebx
	mov ebx, eax
	mov eax, ebx
	shr eax, 24
	call print_hex
	mov eax, ebx
	shr eax, 16
	call print_hex
	mov eax, ebx
	shr eax, 8
	call print_hex
	mov eax, ebx
	call print_hex
	pop ebx
	pop eax
	ret

kill_motor:
	push dx
	mov dx, 0x3F2
	mov al, 0x00
	out dx, al
	pop dx
        ret

reset_drive:
	push ax
	push dx
	mov ax, 0x00
	mov dl, [BOOT_DRIVE]
	int 0x13
	pop dx
	pop ax
        ret

HEX_TEMPLATE:
	db '??', 0

HEX_PREFIX:
	db '0x', 0

HEXABET:
	db '0123456789ABCDEF'

MESSAGE_GREETING:
	db "UFLBBL loading...", 13, 10, 0

MESSAGE_ERROR:
	db "ERR ", 0

SPACE:
	db " ", 0

NL:
	db 13, 10, 0

BOOT_DRIVE:
	db 0

; pad rest of sector with zeroes so we get 512 bytes in the end
	times 510-($-$$) db 0

; magic number of a boot sector
	dw 0xaa55

; stage 2
stage2:

; check A20 gate
	mov si, MESSAGE_CHECKING_A20
	call print_string
	call check_and_enable_A20
	cmp ax, 1
	je .A20_enabled
	mov si, MESSAGE_DISABLED
	call print_string
	mov di, ERR_A20
	mov ax, 0x01
	call print_error

.A20_enabled:
	mov si, MESSAGE_ENABLED
	call print_string

unreal_mode:
; now switching to bigger data and extra segments (so copying kernels and ramdisks
; is not overly painful)
	mov si, MESSAGE_SWITCHING_TO_UNREAL_MODE
	call print_string

; disable interrupts for now as we do mode switching and segment manipulations
	cli
	
; load GDT (global descriptor table)
	lgdt [gdt_descriptor]

; switch to protected mode
	mov eax, cr0
	or eax, 0x1
	mov cr0, eax
	
; unconditional far jump into code segment,
; wipes the instruction prefetch pipeline
	jmp $+2

; data descriptor in GDT
	mov ax, CODE_DATA_SEGMENT
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax

; back to real mode
	and al, 0xFE
	mov cr0, eax

; restore segment values - now limits are removed but seg regs still work as normal
; we keep cs and ss in real mode locations as before, code is small and stack
; is not expected to get big, besides they stop working above 1 MB as SP and IP
; are used and not ESP and EIP!
	xor ax,ax 
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax

; now we are in UNREAL mode, we can also reenable real mode interrupts, as the
; code segment is still small, so saving IP should work just fine
	sti
	mov si, MESSAGE_ENABLED
	call print_string

; detect disk geometry
; IN dl: drive
detect_disk_geometry:
	xor ax, ax
	mov es, ax
	mov di, ax
	mov ah, 0x08
	mov dl, [BOOT_DRIVE]
	int 0x13
	jc .error
	jmp .ok

.error:
; AH contains return code
	mov si, ERR_DISK
	call print_error

; all went well, remember and print drive parameters
.ok:
	mov si, MESSAGE_DRIVE_PARAMETERS
	call print_string
	mov ax, [BOOT_DRIVE]
	call print_hex_nice
	xor ax, ax
	mov [FLOPPY_TYPE], BYTE bl
	mov al, [FLOPPY_TYPE]
	call print_hex_nice
	add dh, 1
	mov [NOF_HEADS], BYTE dh
	add cl, 1
	mov [SECTORS_PER_CYLINDER], BYTE cl
	mov al, [NOF_HEADS]
	call print_hex_nice
	xor ax, ax
	mov al, [SECTORS_PER_CYLINDER]
	call print_hex_nice
	mov si, NL
	call print_string
	
	call reset_drive

; read now sectors after stage 2 as long as we can find
; tar index sectors containing filenames,
; depending on the filename we have to load things to different places
; (and move them to memory above 1MB)
; start to read after stage 1 (512 on sector 1) and stage 2 (starting on sector 2)
	mov [CURRENT_SECTOR], byte NOF_SECTORS_STAGE2 + 2

read_next_sector:

	mov bx, 0x0900			; where to store the data (our floppy data read buffer)
	mov es, bx
	mov bx, 0x0

; print (C/H/S) info where we are currently reading and in which state
; and how many data sectors are left in the current tar entry
	jmp .no_verbose

; states
	mov al, 0x0d
	call print_char
	mov al, byte [READ_STATE]
	call print_hex
	mov al, ' '
	call print_char
	mov al, byte [KERNEL_STATE]
	call print_hex
	mov al, ' '
	call print_char

; tar data sectors to read for current file
	mov dx, word [READ_DATA_SECTORS]
	mov ax, dx
	shr ax, 8
	call print_hex
	mov ax, dx
	call print_hex	
	mov al, ' '
	call print_char
	
; disk geometry (current position on the floppy)	
	mov al, '('
	call print_char
	xor ax, ax
	mov al, byte [CURRENT_CYLINDER]
	call print_hex
	mov al, '/'
	call print_char
	xor ax, ax
	mov al, byte [CURRENT_HEAD]
	call print_hex
	mov al, '/'
	call print_char
	xor ax, ax
	mov al, byte [CURRENT_SECTOR]
	call print_hex
	mov al, ')'
	call print_char

.no_verbose:
	call read_one_sector_from_disk

; states in the main sector read loop
STATE_READ_METADATA equ 0
STATE_READ_KERNEL equ 1
STATE_READ_INITRD equ 2
STATE_KERNEL_READ_SECTOR_0 equ 1
STATE_KERNEL_READ_SECTOR_1 equ 2
STATE_KERNEL_READ_SECTORS_REAL_MODE equ 3
STATE_KERNEL_READ_SECTORS_PROTECTED_MODE equ 4
STATE_KERNEL_FINISHED equ 5

; depending on the read state we have to do different stuff now
	mov ah, byte [READ_STATE]
	cmp ah, STATE_READ_METADATA
	je handle_read_metadata
	cmp ah, STATE_READ_KERNEL
	je handle_read_kernel
	cmp ah, STATE_READ_INITRD
	je handle_read_initrd
	mov di, ERR_DISK
	mov ax, 0x03
	call print_error

; state: STATE_READ_METADATA
handle_read_metadata:
	push bx
	mov esi, 0x09000 + 0x101			; 5 chars must match 'ustar' for being a tar metadata block
	mov edi, USTAR_MAGIC
	mov bx, 5
	call strncmp
	pop bx
	cmp ax, 0
	je .print_tar_metadata
	jmp advance_to_next_sector			; read over unknown data or non-tar entries

.print_tar_metadata:
	mov al, ' '
	call print_char
	mov si, 0x09000 + 0x00				; the filename in the tar
	call print_string
	mov al, ' '
	call print_char
	mov si, 0x09000 + 0x7c				; the size in ASCII octal
	call print_string
	mov si, SPACE
	call print_string
	mov si, 0x09000 + 0x7c
	mov ecx, 11

	; compute size in decimal and compute number of data sectors we have to read
	call octal_string_to_int
	a32 mov [READ_DATA_SIZE], eax
	call print_hex_dword
	
	mov ecx, eax					; ecx contains the number of 512 byte sectors
	shr ecx, 9					; number of sectors a 512 bytes
	mov edx, ebx					; compute if we have a remainder
	and edx, 0x01FF
	cmp edx, 0					; no remainder, ending at full sector
	je .full_sector
	inc ecx						; one non-full sector more to read
.full_sector:
	mov [READ_DATA_SECTORS], ecx

	; enable to debug metadata in tar
	;~ mov si, NL
	;~ call print_string
	;~ mov ax, 0x0900
	;~ mov es, ax
	;~ call print_memory
	
.search_kernel:
	mov esi, 0x09000 + 0x00
	mov edi, FILE_BZIMAGE
	call strcmp
	cmp ax, 0
	je .found_kernel
	jmp .search_initrd

.found_kernel:
	mov [READ_STATE], byte STATE_READ_KERNEL
	mov [KERNEL_STATE], byte STATE_KERNEL_READ_SECTOR_0
	mov [READ_DESTINATION_PTR], dword 0x10000
	mov al, '!'
	call print_char
	mov si, NL
	call print_string
	jmp advance_to_next_sector

.search_initrd:
	mov esi, 0x09000 + 0x00
	mov edi, FILE_RAMDISK
	call strcmp
	cmp ax, 0
	je .found_initrd
	jmp .search_eof

.found_initrd:
	mov [READ_STATE], byte STATE_READ_INITRD
; qemu initrd start location 7fab000, 133869568 (this is 128MB) too high for us,
; kernel gives us alignment hints and hints where to load initrd to?
; let's use 8MB, TODO: does the kernel release the initial ramdisk? I think so. is
; it relocating it's structures? or do we get fragmented heap and stuff?
	a32 mov [READ_DESTINATION_PTR], dword 0x00800000
	a32 mov [INITRD_ADDRESS], dword 0x00800000
	a32 mov eax, [READ_DATA_SIZE]
	a32 mov [INITRD_SIZE], eax
	mov al, '!'
	call print_char
	mov si, NL
	call print_string

	jmp advance_to_next_sector

.search_eof:
	mov esi, 0x09000 + 0x00
	mov edi, FILE_EOF
	call strcmp
	cmp ax, 0
	je .found_eof
	jmp .unknown_file

.found_eof:
	mov si, NL
	call print_string
	mov si, MESSAGE_EOF_REACHED
	call print_string
	jmp finished_reading

.unknown_file:
	mov al, '?'
	call print_char
	mov si, NL
	call print_string
	jmp advance_to_next_sector

; STATE: STATE_READ_KERNEL
handle_read_kernel:

	; move kernel code/data from 0x09000 (our disk read scratch space
	; in low memory to 0x10000 (which is the real mode location for the
	; kernel code)
	mov ax, ds
	mov es, ax
	mov esi, 0x09000
	mov edi, dword [READ_DESTINATION_PTR]
	mov ecx, 512
	cld
	a32 rep movsb
	mov [READ_DESTINATION_PTR], edi
	
	mov ax, word [READ_DATA_SECTORS]
	dec ax
	mov word [READ_DATA_SECTORS], ax
	cmp ax, 0
	je .data_end
	jmp kernel_switch
.data_end:
	mov [READ_STATE], byte STATE_READ_METADATA
	jmp advance_to_next_sector
	
	; depending on the kernel substate
kernel_switch:
	xor ax, ax
	mov ah, byte [KERNEL_STATE]
	cmp ah, STATE_KERNEL_READ_SECTOR_0
	je handle_data_kernel_sector_0
	cmp ah, STATE_KERNEL_READ_SECTOR_1
	je handle_data_kernel_sector_1
	cmp ah, STATE_KERNEL_READ_SECTORS_REAL_MODE
	je handle_data_kernel_real_mode
	cmp ah, STATE_KERNEL_READ_SECTORS_PROTECTED_MODE
	je handle_data_kernel_protected_mode
	mov di, ERR_KERN
	mov ax, 0x01
	call print_error
	
; STATE: STATE_KERNEL_READ_SECTOR_0
handle_data_kernel_sector_0:

; first sector if real mode kernel

	; enable to debug real mode zero page metadata
	;~ mov si, NL
	;~ call print_string
	;~ mov ax, 0x1000
	;~ mov es, ax
	;~ call print_memory

	; get the number of real mode kernel sectors to read
	; (or mininmally 4 if 0 is returned), each 512 bytes
	mov si, NL
	call print_string
	a32 mov al, byte [0x10000+0x1f1]
	cmp al, 0
	jne .nof_sectors_ok
	mov al, 4					; minimally 4 sectors
	
.nof_sectors_ok:
	mov si, MESSAGE_KERNEL_NOF_REAL_SECTORS
	call print_string
	mov [KERNEL_NOF_REAL_MODE_SECTORS], al
	call print_hex
	mov si, NL
	call print_string

	; get size of protected mode kernel in 16 bytes
	a32 mov eax, [0x10000+0x1f4]
	mov ecx, eax
	shr ecx, 5					; 16-pages, shifting by 5 to the right gives
							; us the number of sectors a 512 bytes
	mov ebx, eax					; compute if we have to read one sector more with partial data
	and ebx, 0x001F					; 32 times 16 bytes per page
	cmp ebx, 0
	je .full_sector
	inc ecx
.full_sector:
	mov [KERNEL_NOF_PROTECTED_MODE_SECTORS], word cx
	mov si, MESSAGE_KERNEL_NOF_PROTECTED_SECTORS
	call print_string
	mov bx, [KERNEL_NOF_PROTECTED_MODE_SECTORS]
	mov ax, bx
	and ax, 0xFF00
	shr ax, 8
	call print_hex
	mov ax, bx
	and ax, 0x00FF
	call print_hex
	mov si, NL
	call print_string

	mov [KERNEL_STATE], byte STATE_KERNEL_READ_SECTOR_1

	jmp advance_to_next_sector

; STATE: STATE_KERNEL_READ_SECTOR_1
handle_data_kernel_sector_1:
	; enable to debug real mode zero page metadata
	;~ mov si, NL
	;~ call print_string
	;~ mov ax, 0x1020
	;~ mov es, ax
	;~ call print_memory

	; compare header
	mov esi, 0x10000 + 0x202
	mov edi, KERNEL_MAGIC
	mov bx, 4
	call strncmp
	cmp ax, 0
	je .kernel_HdrS_found
	mov di, ERR_KERN
	mov ax, 0x02
	call print_error
	
.kernel_HdrS_found:
	; get protocol version, don't allow anothing below 2.15 (which
	; os kernel 5.5 or above)
	mov al, 0x0d
	call print_char
	mov si, NL
	call print_string
	mov si, MESSAGE_KERNEL_BOOT_PROTOCOL
	call print_string
	a32 mov al, byte [0x10000+0x207]
	mov byte [KERNEL_BOOT_PROTOCOL_MAJOR], al
	call print_hex	
	mov al, '.'
	call print_char
	a32 mov al, byte [0x10000+0x206]
	mov byte [KERNEL_BOOT_PROTOCOL_MINOR], al
	call print_hex	
	mov si, NL
	call print_string
	xor ax, ax
	mov al, byte [KERNEL_BOOT_PROTOCOL_MAJOR]
	cmp al, 2
	jl .protocol_error
	xor ax, ax
	mov al, byte [KERNEL_BOOT_PROTOCOL_MINOR]
	cmp al, 15
	jl .protocol_error
	jmp .get_kernel_version_offset
	
.protocol_error:
	mov di, ERR_KERN
	mov ax, 0x03
	call print_error

	; get offset pointing to human readable kernel message,
	; we can print it only after having read the real mode part
	; of the kernel (or just before we actually start the kernel)
.get_kernel_version_offset:
	a32 mov ax, word [0x10000+0x20e]
	mov [KERNEL_VERSION_PTR], ax
	jmp .set_boot_data

.set_boot_data:
	; not quite clear what the kernel does with this data, TODO: must read kernel code
	a32 mov byte  [0x10000+0x210], 0xe1	; Extended bootloader type
	a32 mov byte  [0x10000+0x226], 0x00	; ext_loader_ver
	a32 mov byte  [0x10000+0x227], 0x01	; ext_loader_type (id: 0x11)
	a32 or  byte  [0x10000+0x211], 0x80	; set CAN_USE_HEAP
	a32 mov word  [0x10000+0x224], 0xde00 	; head_end_ptr
	
	; set up area and copy command line from the boot loader area to
	; a area registered with the kernel (0x1e000)
	a32 mov	dword [0x10000+0x228], 0x1e000	; set cmd_line_ptr
	xor ax, ax
	mov es, ax
	mov esi, KERNEL_CMD_LINE
	mov edi, 0x1e000
	mov ecx, KERNEL_CMD_SIZE
	cld
	a32 rep movsb
	
	jmp .change_state

.change_state:
	; enable to debug real mode zero page metadata after modifying and setting parameters
	;~ mov si, NL
	;~ call print_string
	;~ mov ax, 0x1020
	;~ mov es, ax
	;~ call print_memory

	mov [KERNEL_STATE], byte STATE_KERNEL_READ_SECTORS_REAL_MODE
	; intentional fallthrough, decrement real mode sectors below

; STATE: STATE_KERNEL_READ_SECTORS_REAL_MODE
handle_data_kernel_real_mode:
	; read at most KERNEL_NOF_REAL_MODE_SECTORS sectors for real
	; mode code/data
	mov al, byte [KERNEL_NOF_REAL_MODE_SECTORS]
	dec al
	mov byte [KERNEL_NOF_REAL_MODE_SECTORS], al
	cmp al, 0
	je .last_real_mode_sector_read
	jmp advance_to_next_sector

.last_real_mode_sector_read:
	; show kernel version
	mov al, 0x0d
	call print_char
	mov si, NL
	call print_string
	mov si, MESSAGE_KERNEL_VERSION
	call print_string
	mov esi, [KERNEL_VERSION_PTR]
	add esi, 0x200
	push ds
	mov ax, 0x1000
	mov ds, ax
	call print_string
	pop ds
	mov si, NL
	call print_string
	
	; change load pointer to protected mode area 0x100000
	mov [KERNEL_STATE], byte STATE_KERNEL_READ_SECTORS_PROTECTED_MODE
	mov [READ_DESTINATION_PTR], dword 0x100000
	
	jmp advance_to_next_sector

; STATE: STATE_KERNEL_READ_SECTORS_PROTECTED_MODE
handle_data_kernel_protected_mode:
	; read at most KERNEL_NOF_PROTECTED_MODE_SECTORS sectors for protected
	; mode code/data
	mov ax, word [KERNEL_NOF_PROTECTED_MODE_SECTORS]
	dec ax
	mov word [KERNEL_NOF_PROTECTED_MODE_SECTORS], ax
	cmp ax, 0
	je .last_protected_mode_sector_read
	jmp advance_to_next_sector
.last_protected_mode_sector_read:
	mov [READ_STATE], byte STATE_READ_METADATA
	mov [KERNEL_STATE], byte STATE_KERNEL_FINISHED
	jmp advance_to_next_sector

; STATE: STATE_READ_INITRD
handle_read_initrd:
	; move ramdisk data from 0x09000 (our disk read scratch space
	; in low memory to high memory 0xaaaa0000
	mov ax, ds
	mov es, ax
	mov esi, 0x09000
	
	mov edi, dword [READ_DESTINATION_PTR]
	mov ecx, 512
	cld
	a32 rep movsb
	mov [READ_DESTINATION_PTR], edi

	mov ax, word [READ_DATA_SECTORS]
	dec ax
	mov word [READ_DATA_SECTORS], ax
	cmp ax, 0
	je .data_end
	jmp .nothing_todo
.data_end:
	mov [READ_STATE], byte STATE_READ_METADATA
	jmp advance_to_next_sector
	
.nothing_todo:

advance_to_next_sector:
	add [CURRENT_SECTOR], byte 1	; next sector
	mov ch, [SECTORS_PER_CYLINDER]
	cmp [CURRENT_SECTOR], ch	; after the end of the current track?
	je .next_head
	jmp read_next_sector

.next_head:
	shr bx, 4			; make it a segment offset..
	mov ax, es
	add ax, bx
	mov es, ax			; ..and add it to ES
	mov bx, 0x0			; we also reset bx and update es to avoid hitting the 64k wrap around point
	mov [CURRENT_SECTOR], byte 1	; start from first sector again
	add [CURRENT_HEAD], byte 1	; advance head
	mov ch, [NOF_HEADS]
	cmp [CURRENT_HEAD], ch		; after the number of heads?
	je .next_track
	jmp read_next_sector
	
.next_track:
	mov [CURRENT_HEAD], byte 0	; start from head 0 again
	add [CURRENT_CYLINDER], byte 1	; advance track
	; TODO depends on boot parameters (the floppy media, add a table)
	cmp [CURRENT_CYLINDER], byte 80
	jae .next_floppy
	jmp read_next_sector

.next_floppy:
	call kill_motor
	mov si, NL
	call print_string
	mov si, MESSAGE_NEXT_FLOPPY
	call print_string
	call wait_for_keypress
	call reset_drive
	; TODO: check for floppy disk change (is there a BIOS function for this?)
	; TODO: maybe also check some checksum or so of the floppy data and see
	; if we indeed have a new floppy
	mov [CURRENT_SECTOR], byte 0	; will be incremented at advance_to_next_sector
	mov [CURRENT_HEAD], byte 0
	mov [CURRENT_CYLINDER], byte 0
	jmp advance_to_next_sector
	
finished_reading:
	; make sure the floppy is not spinnig (also in print_error)
	call kill_motor

start_kernel:

	; set ramdisk
	a32 mov eax, [INITRD_ADDRESS]		; ramdisk size in bytes
	a32 mov [0x10000+0x218], eax
	mov si, MESSAGE_INITRD_ADDRESS
	call print_string
	a32 mov eax, [INITRD_ADDRESS]
	call print_hex_dword
	mov si, NL
	call print_string
	a32 mov eax, [INITRD_SIZE]		; ramdisk address in bytes
	a32 mov [0x10000+0x21c], eax
	mov si, MESSAGE_INITRD_SIZE
	call print_string
	a32 mov eax, [INITRD_SIZE]
	call print_hex_dword
	mov si, NL
	call print_string

	; TODO: check if we actually do have reached the KERNEL_FINISHED state
	mov si, MESSAGE_BOOTING_KERNEL
	call print_string

	; set up segments for running the real mode kernel
	cli
	mov ax, 0x1000
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax
	mov sp, 0xe000

	; jump to kernel real mode entry
	jmp 0x1020:0

; we should not return here, rather the machine will reset, hang or kernel will OUPS,
; just in case, restore segments and print an error message just in case it happens..
	mov ax, CODE_DATA_SEGMENT
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax
	mov sp, 0x7c00
	
	mov si, ERR_KERN
	mov ax, 0x01
	call print_error

; allow rebooting if something goes sour in the kernel
reboot:
	mov si, MESSAGE_REBOOT
	call print_string
	
	call wait_for_keypress

; reboot (cannot use jmp here)
	db 0xea
	dw 0x0000
	dw 0xFFFF

; endless loop if reboot fails
	jmp $

; wait for key to be pressed
wait_for_keypress:
	push ax
	xor ax, ax
	int 0x16
	pop ax
	ret

; GDT global descriptor table

gdt_start:

; mandatory null entry
gdt_null:
	dd 0x0
	dd 0x0

; on big unreal segment, code and data are in the same unprotected segment
gdt_code_data:
	dw 0xffff	; limit (bits 0-15)
	dw 0x0		; base (bits 0-15)
	db 0x0		; base (bits 16-23)
	db 10010010b	; flags
	db 11001111b	; flags, limit (bits 16-19)
	db 0x0		; base (bit 24-31)

gdt_end:

gdt_descriptor:	
	dw gdt_end - gdt_start - 1	; size
	dd gdt_start			; start address of the GDT
	
; constants representing the segment bases
CODE_DATA_SEGMENT equ gdt_code_data - gdt_start

check_and_enable_A20:
	call check_A20_enabled
	cmp ax, 1
	je A20_ENABLED

A20_FAST_SPECIAL_PORT:

	mov al, 'F'
	call print_char

	in al, 0x92
	or al, 2
	out 0x92, al

	call check_A20_enabled
	cmp ax, 1
	je A20_ENABLED
	
A20_ENABLE_KBD_PORT:
	mov al, 'K'
	call print_char
	mov al, 0xdd
	out 0x64, al

	call check_A20_enabled
	cmp ax, 1
	je A20_ENABLED

A20_ENABLE_VIA_BIOS:
	mov al, 'B'
	call print_char
	mov ax, 0x2401
	int 0x15

	call check_A20_enabled
	cmp ax, 1

A20_ENABLE_KBD_OUT:

	mov al, 'k'
	call print_char

	cli			; disable interrupts, we talk directly to the keyboard ports

        call    .wait_input
        mov     al,0xAD
        out     0x64,al		; disable keyboard
        call    .wait_input

        mov     al,0xD0
        out     0x64,al		; tell controller to read output port
        call    .wait_output

        in      al,0x60
        push    eax		; get output port data and store it
        call    .wait_input

        mov     al,0xD1
        out     0x64,al		; tell controller to write output port
        call    .wait_input

        pop     eax
        or      al,2		; set bit 1 (enable a20)
        out     0x60,al		; write out data back to the output port

        call    .wait_input
        mov     al,0xAE		; enable keyboard
        out     0x64,al

        call    .wait_input

        sti

        jmp .retest

; wait for input buffer to be clear
.wait_input:
        in      al,0x64
        test    al,2
        jnz     .wait_input
        ret

; wait for output buffer to be clear
.wait_output:
        in      al,0x64
        test    al,1
        jz      .wait_output
        ret

.retest:
	call check_A20_enabled
	cmp ax, 1
	je A20_ENABLED
  	
A20_ENABLED:
	ret

; returns 0 if not A20_ENABLED, 1 if A20_ENABLED in AX
check_A20_enabled:
	pushf
	push ds
	push es
	push di
	push si
	
	cli
	
	xor ax, ax
	mov es, ax
	mov di, 0x0500			; es:di = 0000:0500
	
	mov ax, 0xffff
	mov ds, ax
	mov si, 0x0510			; ds:si = ffff:0510
	
	mov al, byte [es:di]		; preserve values in memory
	push ax				; on stack
	mov al, byte [ds:si]
	push ax
	
	mov byte [es:di], 0x00		; now the test: write 0x00 to 0000:0500
	mov byte [ds:si], 0xFF		; write 0xff to ffff:0510
	
	cmp byte [es:di], 0xFF		; memory wrap? A20 not enabled
	je .disabled
	jmp .enabled

.restore:
	pop bx				; restore original memory contents
	mov byte [ds:si], bl
	pop bx
	mov byte [es:di], bl
	jmp .exit
		
.enabled:
	mov al, '+'
	call print_char
	mov ax, 1			; not wrapped around
	jmp .restore

.disabled:
	mov al, '-'
	call print_char
	mov ax, 0			; wrapped around (last cmp)
	jmp .restore
	
.exit:
	pop si
	pop di
	pop es
	pop ds
	popf

	ret

; Read one sector from floppy using CHS adressing
read_one_sector_from_disk:

	mov ah, 0x02			; read sectors from drive
	mov al, 1			; read 1 sector
	mov ch, BYTE [CURRENT_CYLINDER]
	mov dh, BYTE [CURRENT_HEAD]
	mov dl, BYTE [BOOT_DRIVE]
	mov cl, BYTE [CURRENT_SECTOR]
	mov BYTE [CURRENT_RETRIES], 0
			
	int 0x13
	
	jc .read_error
	
	cmp al, 1			; 1 sector read?
	jne .short_read			; if not, short read
	
	ret
	
.read_error:
	cmp BYTE [CURRENT_RETRIES], 3
	jl .read_again
	jmp .print_error
.read_again:
	dec BYTE [CURRENT_RETRIES]
	call reset_drive
	jmp read_one_sector_from_disk
	
.print_error:
	;~ xor ax, ax
	;~ mov dh, 0
	;~ mov dl, ah
	mov di, ERR_DISK
	call print_error

.short_read:
	mov di, ERR_DISK
	call print_string

; IN al: character to print if printable, dot otherwise
; MOD ah
print_dump_char:
	cmp byte al, 0x20		; space
	jl .print_dot
	test byte al, al		; above 0x7f ~
	js .print_dot
	call print_char
	jmp .done
.print_dot:
	mov al, 0x2e			; .
	call print_char
.done:
	ret

; print a dump of 512 bytes in memory (a sector) for debugging purposes
; IN: es base address (as segment) where to start printing
print_memory:
	xor bx, bx
.next_line:
	xor cx, cx
	mov ax, es
	and ax, 0xFF00
	shr ax, 8
	call print_hex
	mov ax, es
	and ax, 0x00FF
	call print_hex
	xor ax, ax
	mov ah, ':'
	call print_char
	mov al, bh
	call print_hex
	mov al, bl
	call print_hex
	mov si, DUMP_SEP
	call print_string
.next_byte:
	mov al, [es:bx]
	call print_hex
	mov si, SPACE
	call print_string
	inc bx
	inc cx
	cmp cx, 8
	je .gap
	jmp .after_gap
.gap:
	mov si, SPACE
	call print_string
.after_gap:
	cmp cx, 16
	jne .next_byte
.print_chars:
	xor cx, cx
	sub bx, 16
.next_char:
	mov al, [es:bx]
	call print_dump_char
	inc bx
	inc cx
	cmp cx, 16
	jne .next_char
.newline:
	mov si, NL
	call print_string
	cmp bx, 256
	je .wait
	jmp .cont
.wait:
	mov si, MESSAGE_PRESS_KEY_TO_CONTINUE
	call print_string
	call wait_for_keypress	
.cont:
	cmp bx, 512
	jne .next_line
	ret

DUMP_SEP:
	db ": ", 0

; compare strings
; IN: si, di: start of two zero terminated strings
; OUT: ax <0 si < di, >0 si > di; =0 si = di
; CLOBBERS: si, di
strcmp:
	push dx
.loop:
	a32 mov dl, [esi]
	a32 mov dh, [edi]
	cmp dh, 0
	je .done
	cmp dl, 0
	je .done
	cmp dh, dl
	jne .done
	inc si
	inc di
	jmp .loop
.done:
	xor ax, ax
	mov al, byte dh
	sub al, byte dl
.end:
	pop dx
	ret

; compare strings up to a maximal number of characters
; IN: si, di: start of two zero terminated strings, bx: number of chars
; OUT: ax <0 si < di, >0 si > di; =0 si = di
; CLOBBERS: si, di, bx
strncmp:
	push dx
.loop:
	a32 mov dl, [esi]
	a32 mov dh, [edi]
	cmp dh, 0
	je .done
	cmp dl, 0
	je .done
	cmp dh, dl
	jne .done
	inc si
	inc di
	dec bx
	jz .done
	jmp .loop
.done:
	xor ax, ax
	mov al, byte dh
	sub al, byte dl
.end:
	pop dx
	ret
	
; convert octal string to integer
; IN: si, cx: pointer to the string and length of the string
; OUT: eax: converted integer
; clobbers: ebx
octal_string_to_int:
	xor ebx, ebx
.next:
	movzx eax, byte [esi]
	inc esi
	sub al, '0'		; assuming ASCII
	shl ebx, 3		; octal, multiply by 8
	add ebx, eax		; add current digit
	loop .next		; cx--
	mov eax, ebx
	ret
	
MESSAGE_CHECKING_A20:
	db "Checking A20 address gate.. ", 0
	
MESSAGE_ENABLED:
	db " enabled", 13, 10, 0
	
MESSAGE_DISABLED:
	db " disabled", 13, 10, 0

MESSAGE_SWITCHING_TO_UNREAL_MODE:
	db "Switching to unreal mode..", 0

MESSAGE_DRIVE_PARAMETERS:
	db "Boot parameters ", 0

MESSAGE_EOF_REACHED:
	db "Reached end of tar file..", 13, 10, 0
	
MESSAGE_BOOTING_KERNEL:
	db "Booting kernel..", 13, 10, 0

MESSAGE_REBOOT:
	db "Press any key for rebooting..", 13, 10, 0

MESSAGE_NEXT_FLOPPY:
	db "Insert next floppy and press any key to continue..", 13, 10, 0
	
MESSAGE_PRESS_KEY_TO_CONTINUE:
	db "Press any key to continue..", 13, 10, 0

MESSAGE_KERNEL_NOF_REAL_SECTORS:
	db "Number of real-mode kernel sectors: ", 0

MESSAGE_KERNEL_NOF_PROTECTED_SECTORS:
	db "Number of protected-mode kernel sectors: ", 0

MESSAGE_KERNEL_BOOT_PROTOCOL:
	db "Linux boot protocol version: ", 0

MESSAGE_KERNEL_VERSION:
	db "Linux kernel version: ", 0
	
MESSAGE_INITRD_ADDRESS:
	db "Ramdisk address: ", 0

MESSAGE_INITRD_SIZE:
	db "Ramdisk size: ", 0

ERR_A20:
	db "A20 ", 0

ERR_DISK:
	db "DISK ", 0

ERR_KERN:
	db "KERN ", 0

USTAR_MAGIC:
	db "ustar", 0

KERNEL_MAGIC:
	db 'HdrS', 0

; data sections used for reading from floppy, default to some sane values
; get probed and filled in during floppy drive probing
FLOPPY_TYPE:
	db 0x00				; drive type, usually 0x04 after probing for 3 1/4" 1.44MB

SECTORS_PER_CYLINDER:
	db 0x3F				; detect parameters enters the correct value here (sectors + 1)
					; if detection fails, force int13 to read ahead
NOF_HEADS:
	db 0x01				; number of heads + 1

; read route reads and updates those values while reading from floppy
CURRENT_SECTOR:
	db 1

CURRENT_CYLINDER:
	db 0

CURRENT_HEAD:
	db 0				

CURRENT_RETRIES:
	db 0

FILE_BZIMAGE:
	db "bzImage", 0

FILE_RAMDISK:
	db "ramdisk.img", 0

FILE_EOF:
	db "EOF", 0
	
READ_STATE:
	db STATE_READ_METADATA

READ_DATA_SECTORS:
	dd 0

READ_DESTINATION_PTR:
	dd 0x10000

READ_DATA_SIZE:
	dd 0

KERNEL_STATE:
	db STATE_KERNEL_READ_SECTOR_0

KERNEL_NOF_REAL_MODE_SECTORS:
	db 0

KERNEL_NOF_PROTECTED_MODE_SECTORS:
	dw 0

KERNEL_CMD_LINE:
	db "debug loglevel=7 earlycon=uart8250,io,0x3f8,9600n8 console=tty0 console=ttyS0,9600n8 rdinit=/bin/sinit root=/dev/ram0 rootfstype=ramfs iommu=off", 0

KERNEL_CMD_SIZE equ $-KERNEL_CMD_LINE

KERNEL_BOOT_PROTOCOL_MAJOR:
	db 0

KERNEL_BOOT_PROTOCOL_MINOR:
	db 0

KERNEL_VERSION_PTR:
	dw 0

INITRD_ADDRESS:
	dd 0

INITRD_SIZE:
	dd 0

; make sure we have full sectors
times (NOF_SECTORS_STAGE2+1)*512-($-$$) db 0
