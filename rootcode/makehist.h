#ifdef __cplusplus
extern "C" {
#endif

#undef ROOT_TYPE_DECLARATION
#define ROOT_TYPE_DECLARATION(Type) \
typedef struct Type ## _tag Type ## _t; \
typedef Type ## _t * Type ## _p; \
typedef Type ## _t const* const_ ## Type ## _p

  ROOT_TYPE_DECLARATION(TH1F);


  void TH1F_Fill(TH1F_p p, double val);

  TH1F_p read_histogram_from_file( const char* filename, const char* histname ); 

  void write_histogram_to_file( TH1F_p p, const char* filename ); 



  //  void tvpiFree(Domain_p d);

  //  void tvpiJoin(Domain_p d1, Domain_p d2);



  
  int makehist( const char* histname, const char* filename, 
		double start, double end, int numbin, int value[] ); 

  int plothist( const char* histname, const char* filename,
		const char* outfilename,
	      const char* title,
	      const char* xtitle, 
	      const char* ytitle, 
	      int plotfiletype );
  
  int testfunptr( double funptr( double*, double* ) );
  
#ifdef __cplusplus
}
#endif
