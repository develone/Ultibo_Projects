int sam, lf, rh, dwt;
int fwd;
struct timeval currentTime,start,end;
//long start_sec;
//long end_sec;
long mtime, seconds, useconds;
int i;
double sqrt(double x);
int lift(int sam, int lf, int rh, int fwd);
void delay(int dd);
typedef struct {
	int	m_w, m_h;
	int	*m_red, *m_green, *m_blue, *m_tmp;
	int	data[1];
} IMAGE, *IMAGEP;
