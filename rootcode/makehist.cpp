//#include <stdio.h>
#include <iostream>

#include "makehist.h"


#include "TMath.h"
#include "TFile.h"
#include "TStyle.h"
#include "TLatex.h"
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
  cout << "recording root file: " << filename << endl; 
  
  TFile f(filename,"new"); 

  TH1F* hist = new TH1F(histname, histname , numbin, start, end ) ;
  Double_t dvalues[1000] ; 

  for( int i= 0 ; i < numbin ; i++ ) {
    dvalues[i] = values[i] ; 
  } 

  hist->SetContent( dvalues ) ; 

  hist->Write(); 
  
  
}

extern int plothist( const char* histname, const char* filename,
		     const char* outfilename,
		     const char* title,
		     const char* xtitle, 
		     const char* ytitle, 
		     int plotfiletype )
{
  //  printf("haha\n");
  cout << "reading root file: " << filename <<  endl; 
  
  TFile f(filename); 

  TH1F* hist = (TH1F*)f.Get(histname) ;
  if (hist == 0 ) {
    cout << "no histogram with name : " << histname << endl; 
    return -1 ;
  }
  
  TCanvas* c1 = new TCanvas( "mycanvas","mycanvas" ) ; 

  //  c1->UseCurrentStyle();
  c1->cd(1); 
  //  gStyle-> SetCanvasColor(kWhite); 
  c1->SetBorderMode(0); 
  c1->SetFillStyle(1001); 
  c1->SetFillColor(10);  
  //c1->SetFillColor(kWhite);

  // // example code
  //  hist->SetTitle("W^{+}W^{-} #rightarrow l^{+} l^{-} #nu #nu"); 
  //  hist->GetXaxis()->SetTitle("m_{ll} (GeV)");
  //  hist->GetYaxis()->SetTitle("Event / 40 GeV"); 
  //  hist->SetStats(kFALSE); 
  //  hist->Draw(); 

  hist->SetTitle(title); 
  hist->GetXaxis()->SetTitle(xtitle);
  hist->GetYaxis()->SetTitle(ytitle); 
  hist->SetStats(kFALSE); 
  hist->Draw(); 


  


  c1->SaveAs(outfilename);

  cout << "Completed! :) " << endl; 
  delete c1; 
  delete hist; 
  //  delete title;


}
