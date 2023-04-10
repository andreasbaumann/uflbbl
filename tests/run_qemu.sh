#!/bin/sh

sleep 2 && \
qemu-system-i386 -cpu 486 -m 128M \
	-drive "file=floppy00,if=floppy,format=raw" \
	-netdev user,id=net0,net=10.0.0.0/24,host=10.0.0.2,dhcpstart=10.0.0.16,hostfwd=tcp::8080-:80,hostfwd=udp::8081-:81 \
	-device rtl8139,netdev=net0
