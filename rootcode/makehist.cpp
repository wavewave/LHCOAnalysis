//#include <stdio.h>
#include <iostream>

#include "makehist.h"


#include "TMath.h"
#include "TFile.h"
#include "TTree.h"
#include "TChain.h"
#include "TH1F.h"
#include "TCanvas.h"
#include "TLorentzVector.h"
#include "TStyle.h" 

using namespace std;



extern int makehist( const char* histname, const char* filename, 
		     double start, double end, int numbin, int values[]  )
{
  //  printf("haha\n");
  cout << "recording root file" << endl; 
  
  TFile f(filename,"new"); 

  TH1F* hist = new TH1F(histname, histname , numbin, start, end ) ;
  Double_t dvalues[1000] ; 

  for( int i= 0 ; i < numbin ; i++ ) {
    dvalues[i] = values[i] ; 
  } 

  hist->SetContent( dvalues ) ; 

  hist->Write(); 
  
  
}
