#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/prctl.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>

// return non zero if the fd is ready to read
int unix_socket_is_ready(int fd) {
        char ch;
        int ret = recv(fd, &ch, 1, MSG_DONTWAIT | MSG_PEEK);
        if (ret < 0) {
                if (errno == EAGAIN || errno == EWOULDBLOCK || errno == ECONNRESET) {
                        return 0;
                }
                perror("RECV failed while checking if socket is ready");
                exit(1);
        }
        return 1;
}

int unix_socket_errno () {
        return errno;
}
