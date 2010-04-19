#ifdef __cplusplus
extern "C" {
#endif
  
int makehist( const char* histname, const char* filename, 
	      double start, double end, int numbin, int value[] ); 

int plothist( const char* histname, const char* filename,
	      const char* outfilename,
	      const char* title,
	      const char* xtitle, 
	      const char* ytitle, 
	      int plotfiletype );

#ifdef __cplusplus
}
#endif
