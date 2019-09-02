#include <termio.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <poll.h>
#include <errno.h>

struct termios t_old;

/*Set the terminal back when the process exited*/
void do_exit(void)
{
        tcsetattr(STDIN_FILENO, TCSANOW, &t_old);
}

void deal_signal(int sig)
{
	switch(sig) {
		case SIGPIPE:
			do_exit();
			break;
		default:
			break;
	}
}

int main()
{
        char c;
        int ret;
        struct termios t;
        struct sigaction sa;
        struct pollfd pf[2];

        sa.sa_handler = deal_signal;
        sa.sa_flags = 0;
        sigemptyset(&sa.sa_mask);
        sigaction(SIGPIPE, &sa, NULL);

        tcgetattr(STDIN_FILENO, &t);
        memcpy(&t_old, &t, sizeof(struct termios));
        atexit(do_exit);
        cfmakeraw(&t);
        tcsetattr(STDIN_FILENO, TCSANOW, &t);

        while(1) {
                pf[0].fd = STDIN_FILENO;
                pf[0].events = POLLIN;
                pf[1].fd = STDOUT_FILENO;
                pf[1].events = 0;
                if(poll(pf,2,-1)>0) {
                        if(pf[1].revents & POLLERR)
                                break;
                }
                ret = read(STDIN_FILENO, &c, 1);
                if(ret == 0)
                        break;
                if(ret < 0)
                        continue;
                if(write(STDOUT_FILENO, &c, 1) == 1)
                        ;
        }
        return 0;
}
