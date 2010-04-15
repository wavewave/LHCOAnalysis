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



extern int makehist( void  ) //  const char* histname, const char* filename ) 
{
  //  printf("haha\n");
  cout << "haha" << endl; 
  
  TFile f("hist.root","new"); 

  TH1F* hist = new TH1F("E+E-", "test" , 50, 0, 1000 ) ;
  Double_t values[50] ; 

  for( int i= 0 ; i < 50 ; i++ ) {
    values[i] = i ; 
  }

  hist->SetContent( values ) ; 

  hist->Write(); 
  
  
}
