#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <stdio.h>

#include "local/include/liburing.h"


int libloop_socket(int domain, int type, int protocol) {
  int fd = socket(domain, type, protocol);
  int status = fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) | O_NONBLOCK);
  return fd;
}

int libloop_socket_bind4(int fd, char * ipv4, int port){
  struct sockaddr_in address;
  int value = 1;

  // TODO: do it somewhere that is nice...
  if (setsockopt (fd, SOL_SOCKET, SO_REUSEADDR, (char*) &value, sizeof(value)) < 0)
    return -1;

  bzero(&address, sizeof(address));
  address.sin_family = AF_INET;
  address.sin_port = htons(port);

  address.sin_addr.s_addr = inet_addr(ipv4);

  return bind(fd, (struct sockaddr *) &address, sizeof address);
}

void libloop_socket_connect4(struct io_uring_sqe *sqe, int fd, char * ipv4, int port){
  struct sockaddr_in address;

  bzero(&address, sizeof(address));
  address.sin_family = AF_INET;
  address.sin_port = htons(port);
  address.sin_addr.s_addr = inet_addr(ipv4);

  io_uring_prep_connect(sqe, fd, (struct sockaddr *) &address, sizeof address);

}

int libloop_socket_sync_connect4(int fd, char * ipv4, int port){
  struct sockaddr_in address;

  bzero(&address, sizeof(address));
  address.sin_family = AF_INET;
  address.sin_port = htons(port);
  address.sin_addr.s_addr = inet_addr(ipv4);

  return connect(fd, (struct sockaddr *) &address, sizeof address);
}

void libloop_io_uring_sqe_set_flags(struct io_uring_sqe *sqe,
                                    unsigned flags)
{
  io_uring_sqe_set_flags(sqe, flags);
}

/*
 * Assign a 64-bit value to this sqe, which can get retrieved at completion
 * time with io_uring_cqe_get_data64. Just like the non-64 variants, except
 * these store a 64-bit type rather than a data pointer.
 */
void libloop_io_uring_sqe_set_data64(struct io_uring_sqe *sqe,
                                     __u64 data)
{
  io_uring_sqe_set_data64(sqe, data);
}

__u64 libloop_io_uring_cqe_get_data64(const struct io_uring_cqe **cqe)
{
  return io_uring_cqe_get_data64(*cqe);
}

__s32 libloop_io_uring_cqe_get_res(const struct io_uring_cqe **cqe)
{
  return (*cqe)->res;
}

/*
 * Return an IO completion, waiting for it if necessary. Returns 0 with
 * cqe_ptr filled in on success, -errno on failure.
 */
int libloop_io_uring_wait_cqe(struct io_uring *ring,
                              struct io_uring_cqe **cqe_ptr)
{
  return io_uring_wait_cqe(ring, cqe_ptr);
}

int libloop_io_uring_peek_cqe(struct io_uring *ring,
                              struct io_uring_cqe **cqe_ptr)
{
  return io_uring_peek_cqe(ring, cqe_ptr);
}

void libloop_io_uring_cqe_seen(struct io_uring *ring, struct io_uring_cqe *cqe) {
  io_uring_cqe_seen(ring, cqe);
}

void libloop_io_uring_prep_socket(struct io_uring_sqe *sqe, int domain,
                                  int type, int protocol,
                                  unsigned int flags)
{
  io_uring_prep_socket(sqe, domain, type, protocol, flags);
}


void libloop_io_uring_prep_accept(struct io_uring_sqe *sqe, int fd,
                                  struct sockaddr *addr,
                                  socklen_t *addrlen, int flags)
{
  io_uring_prep_accept(sqe, fd, addr, addrlen, flags);
}


void libloop_io_uring_prep_connect(struct io_uring_sqe *sqe, int fd,
                                   const struct sockaddr *addr,
                                   socklen_t addrlen)
{
  io_uring_prep_connect(sqe, fd, addr, addrlen);
}

void libloop_io_uring_prep_timeout(struct io_uring_sqe *sqe,
                                   struct __kernel_timespec *ts,
                                   unsigned count, unsigned flags)
{
  io_uring_prep_timeout(sqe, ts, count, flags);
}

void libloop_io_uring_prep_read(struct io_uring_sqe *sqe, int fd,
                                void *buf, unsigned nbytes, __u64 offset)
{
  io_uring_prep_read(sqe, fd, buf, nbytes, offset);
}

void libloop_io_uring_prep_write(struct io_uring_sqe *sqe, int fd,
                                 const void *buf, unsigned nbytes,
                                 __u64 offset)
{
  io_uring_prep_write(sqe, fd, buf, nbytes, offset);
}


void libloop_io_uring_prep_send(struct io_uring_sqe *sqe, int sockfd,
                                const void *buf, size_t len, int flags)
{
  io_uring_prep_send(sqe, sockfd, buf, len, flags);
}


void libloop_io_uring_prep_recv(struct io_uring_sqe *sqe, int sockfd,
                                void *buf, size_t len, int flags)
{
  io_uring_prep_recv(sqe, sockfd, buf, len, flags);
}


void libloop_io_uring_prep_sync_file_range(struct io_uring_sqe *sqe,
                                           int fd, unsigned len,
                                           __u64 offset, int flags)
{
  io_uring_prep_sync_file_range(sqe, fd, len, offset, flags);
}

void libloop_io_uring_prep_sendmsg(struct io_uring_sqe *sqe, int fd,
                                   const struct msghdr *msg,
                                   unsigned flags)
{
  io_uring_prep_sendmsg(sqe, fd, msg, flags);
}


/* See man 2 recvmsg */

/* struct msghdr { */
/*   void         *msg_name;       /\* Optional address *\/ */
/*   socklen_t     msg_namelen;    /\* Size of address *\/ */
/*   struct iovec *msg_iov;        /\* Scatter/gather array *\/ */
/*   size_t        msg_iovlen;     /\* # elements in msg_iov *\/ */
/*   void         *msg_control;    /\* Ancillary data, see below *\/ */
/*   size_t        msg_controllen; /\* Ancillary data buffer len *\/ */
/*   int           msg_flags;      /\* Flags on received message *\/ */
/* }; */


void libloop_io_uring_prep_recvmsg(struct io_uring_sqe *sqe, int fd,
                                   struct msghdr *msg, unsigned flags)
{
  io_uring_prep_recvmsg(sqe, fd, msg, flags);
}


void libloop_io_uring_prep_shutdown(struct io_uring_sqe *sqe, int fd,
                                    int how)
{
  io_uring_prep_shutdown(sqe, fd, how);
}

void libloop_io_uring_prep_cancel64(struct io_uring_sqe *sqe,
                                    __u64 user_data, int flags)
{
  io_uring_prep_cancel64(sqe, user_data, flags);
}
