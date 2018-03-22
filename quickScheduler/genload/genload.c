#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/types.h>
#include <regex.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define NUM_FIELDS 4

char log_file[16*1024][1024];  // FIXME - Això hauria de ser un valor de mida...
char* address;

typedef struct
{
	int index;
	int entries;
} params;

int formatLine(char* logEntryLine, char** retval)
{
	regex_t regex;
	regmatch_t pmatch;

	// IP ADDRESS
	if (!regcomp(&regex, "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+",REG_EXTENDED)) regexec(&regex, logEntryLine, 1, &pmatch, 0);
	retval[0] = strndup(logEntryLine + pmatch.rm_so, pmatch.rm_eo);
	regfree(&regex);

	// DATE AND TIME
	if (!regcomp(&regex, "\\[[0-9]{2}\\/[a-zA-Z]{3}\\/[0-9]{4}:[0-9]{2}:[0-9]{2}:[0-9]{2}", REG_EXTENDED)) regexec(&regex, logEntryLine, 1, &pmatch, 0);
	retval[1] = strndup(logEntryLine + pmatch.rm_so+1, pmatch.rm_eo - pmatch.rm_so - 1);
	regfree(&regex);

	// METHOD
	if (!regcomp(&regex, " \"[a-zA-Z]+ ", REG_EXTENDED)) regexec(&regex, logEntryLine, 1, &pmatch, 0);
	retval[2] = strndup(logEntryLine + pmatch.rm_so+2, pmatch.rm_eo - pmatch.rm_so -3);
	regfree(&regex);

	// PATH
	if (!regcomp(&regex, " \\/[-a-zA-Z0-9\\_\\.\\:\\&\\?\\%\\=\\+\\/\\]* ", REG_EXTENDED)) regexec(&regex, logEntryLine, 1, &pmatch, 0);
	retval[3] = strndup(logEntryLine + pmatch.rm_so+1, pmatch.rm_eo - pmatch.rm_so -2);
	regfree(&regex);

	return 0;
}

void *threadFunc (void * args)
{
	params * p = (params*)args;
	int id = p->index;
	int entries = p->entries;

	//printf("Hi, I'm Pinkie Pie and I threw this thread just for you! %d\n", id);

	long old_time = -1;
	double mbs = 0;
	int queries = 0;
	double rt = 0;
	double bytes = 0;
	int timeunit = 0;
	//regmatch_t pmatch;
	//regex_t regex;

	int i = 0;
	for (i=0; i < entries; i++)
	{
		char line[1024];
		sprintf(line,"%s",log_file[i]);

		char* parsed_line[NUM_FIELDS] = {NULL};
		int c = 0;
		char* saveptr;
		char* pch = strtok_r (line," ",&saveptr);

		while (pch != NULL)
		{	
			parsed_line[c++] = pch;
			pch = strtok_r (NULL, " ",&saveptr);
		}

		if (strstr(parsed_line[3],"captcha") == NULL)
		{
			struct tm tm;
			time_t time1;
			if (strptime(parsed_line[1], "%d/%b/%Y:%H:%M:%S", &tm)) time1 = mktime(&tm);

			if (old_time == -1) old_time = time1;
			if (time1 - old_time > 0) sleep(time1 - old_time);
			old_time = time1;

			char* p = strstr(parsed_line[2],"GET");

			if (p != NULL)
			{
				char param1[128]; char param2[64];
				sprintf(param1,"%s%s",address,parsed_line[3]);
				sprintf(param2,"--user-agent=\"%d.%s\"",id,parsed_line[0]);
				char* param3 = "-O/dev/null";
				char* param4 = "~";
				char* param5 = "-q";
				char* param6 = "--tries=5";

				//printf("wget %s %s %s %s %d %d\n",param1,param2,param3,param4,id,i);

				/* Crida al wget */
				pid_t pid;
				int fd[2], nbytes;
				pipe(fd);
				char readbuffer[1024];

				pid = fork();
				if (pid < 0)
				{
					continue;
				}
				else if (pid == 0)
				{
					close(fd[0]);
					dup2(fd[1], 1);
					dup2(fd[2], 1);
					close(fd[1]);
					close(fd[2]);

					char cmd1[] = "/usr/bin/wget"; char *args1[] = {"wget",param1,param2,param6,param5,param3,param4, NULL};
					execvp(cmd1,args1);
					printf("de moment tot va malament...\n");
					exit(0);
				}
				else
				{
		            close(fd[1]);
					nbytes = read(fd[0], readbuffer, sizeof(readbuffer));
					readbuffer[nbytes]='\0';
		            close(fd[0]);
				}
				queries++;
			}
			/* FINAL GET */
		}
		/* FINAL QUERY */
		//printf("I'm (%d %d) and I'm done with %s\n", id, i,parsed_line[3]);
	}
	/* FINAL ENTRIES */
	//printf("Thread %d Executades %d linies\n",id,i);
}

int main ( int argc, char** argv )
{
	char* log_file_arg = argv[1];	// "./imageboard-test-27-11-2010.log";
	address = argv[2];				// "http://blade3:8080";
	int mult = atoi(argv[3]);		// 1000
	int offset = atoi(argv[4]);		// 100

	/* Num linies del fitxer */
	FILE *f = fopen(log_file_arg,"rb");
	int c = 0, b;
	while ((b=fgetc(f))!=EOF) c+=(b==10)?1:0;
	fseek(f,0,SEEK_SET);
	fclose(f);

	//printf("Log Entries: %d\n",c);

	/* Llegeix el fitxer */
	FILE *file = fopen(log_file_arg,"r");
	if ( file != NULL )
	{
		int count = 0;
		char line[1024];
		while ( fgets (line,sizeof(line),file) != NULL && count < c)
		{
			char* aux[NUM_FIELDS];
			formatLine(line, aux);
			sprintf(log_file[count],"%s %s %s %s",aux[0],aux[1],aux[2],aux[3]);
			int i;
			for(i = 0; i < NUM_FIELDS; i++) free(aux[i]);
			count++;
		}
		fclose ( file );
	} else {
		perror ( log_file_arg );
		return -1;
	}

	/* Executa Threads */
	pthread_t pth[mult];
	int i = 0;

	//for (i=0; i < 10; i++) printf("A: %s\n",log_file[i]);
	//exit(0);

	for (i=0; i < mult; i++)
	{
		params p;
		p.index = i;
		p.entries = c;

		pthread_create(&(pth[i]),NULL,threadFunc,&p);
		usleep(1000);
	}

	/* Recull Threads */
	for (i=0; i<mult; i++)
	{
		pthread_join(pth[i],NULL);
	}

	//printf("END GENLOAD\n");

	return 0;
}
