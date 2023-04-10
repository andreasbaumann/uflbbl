#include <stdio.h>
#include <stdlib.h>
#include <bsd/string.h>
#include <string.h>

static int octal_to_int( char *s, int size )
{
	int n = 0;
	char *c = s;
	while( size > 0 ) {
		n *= 8;
		n += *c - '0';
		c++;
		size--;
	}
	
	return n;
}

int main( int argc, char *argv[] )
{
	FILE *f = NULL;
	char buf[512];
	char filename[100];
	char size_octal[12];
	char cksum_octal[8];
	int size;
	unsigned int cksum;
	unsigned int computed_cksum;
	int i, n;
	int terminate = 0;

	if( argc != 2 ) {
		fprintf( stderr, "usage: %s <tarfile>\n", argv[0] );
		exit( EXIT_FAILURE );
	}
	
	f = fopen( argv[1], "r" );
	if( f == NULL ) {
		fprintf( stderr, "ERROR: failed to open file '%s'\n", argv[1] );
		exit( EXIT_FAILURE );
	}
	
	do {
		clearerr( f );
		n = fread( buf, 512, 1, f );
		if( n == 0 ) {
			if( ferror( f ) ) {
				fprintf( stderr, "ERR: read error\n" );
				terminate = 1;
			} else if( feof( f ) ) {
				terminate = 1;
				break;
			}
		}
		
		if( memcmp( &buf[257], "ustar", 5 ) == 0 ) {
			strlcpy( filename, buf, 100 );
			
			if( strcmp( filename, "" ) != 0 ) {

				printf( "file: %s\n", filename );
			
				strlcpy( size_octal, &buf[124], 12 );
				printf( "size (octal): %s\n", size_octal );

				size = octal_to_int( size_octal, 11 );
				printf( "size (decimal): %d\n", size );

				strlcpy( cksum_octal, &buf[148], 8 );
				printf( "cksum (octal): %s\n", cksum_octal );
				cksum = octal_to_int( cksum_octal, 6 );
				printf( "cksum (decimal): %d\n", cksum );
				
				for( i = 148; i < 148+8; i++ ) {
					buf[i] = ' ';
				}
				computed_cksum = 0;
				for( i = 0; i < 512; i++ ) {
					computed_cksum += (unsigned char)buf[i];
				}
				printf( "cksum (computed): %d\n", computed_cksum );
				if( cksum != computed_cksum ) {
					fprintf( stderr, "ERR: checksum mismatch in header!\n" );
				}

				while( size > 0 ) {
					fread( buf, 512, 1, f );
					size -= 512;
				}
			} else {
				fprintf( stderr, "INFO: the end\n" );
			}			
		} else {
			fprintf( stderr, "WARN: skipping non TAR header\n" );
		}
	} while( !terminate );
	
	fclose( f );
	
	exit( EXIT_SUCCESS );
}
