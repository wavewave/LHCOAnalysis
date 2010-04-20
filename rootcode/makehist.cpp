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


template<class ToType, class FromType>
const ToType* to_const(const FromType* x) {
  return reinterpret_cast<const ToType*>(x);
}

template<class ToType, class FromType>
ToType* to_nonconst(FromType* x) {
  return reinterpret_cast<ToType*>(x);
}


void TH1F_Fill( TH1F_p p, double val ) {
 
  //  cout << " in Fill, p = " << p << endl;
  to_nonconst<TH1F, TH1F_t>(p)->Fill( val );

  //   cout << "hist fill" << endl;   
}

TH1F_p read_histogram_from_file( const char* filename, const char* histname )
{
  TFile* f = new TFile(filename); 
  TH1F* hist = 0 ; 
  hist = (TH1F*)(f->Get(histname)) ; 
  cout << "hist = " << hist << endl; 

  return  to_nonconst<TH1F_t, TH1F>(hist);
}

void write_histogram_to_file( TH1F_p p, const char* filename ) 
{
  TFile f(filename,"new"); 
  cout << " p = " << p << endl; 
  to_nonconst<TH1F, TH1F_t>(p)->Write("test"  );
  cout << "hist write" << endl; 
}


int makehist( const char* histname, const char* filename, 
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

int plothist( const char* histname, const char* filename,
		     const char* outfilename,
		     const char* title,
		     const char* xtitle, 
		     const char* ytitle, 
		     int plotfiletype )
{
  cout << "reading root file: " << filename <<  endl; 
  
  TFile f(filename); 

  TH1F* hist = (TH1F*)f.Get(histname) ;
  if (hist == 0 ) {
    cout << "no histogram with name : " << histname << endl; 
    return -1 ;
  }
  
  TCanvas* c1 = new TCanvas( "mycanvas","mycanvas" ) ; 

  c1->cd(1); 
  c1->SetBorderMode(0); 
  c1->SetFillStyle(1001); 
  c1->SetFillColor(10);  

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
  return 0; 
}


/*
int plothist_with_fitting( const char* histname, 
			   const char* filename,
			   const char* outfilename,
			   const char* title,
			   const char* xtitle, 
			   const char* ytitle, 
			   int plotfiletype 
			   double funptr( double, double,double, double ) 
			   )
{
  Double_t myfunc( Double_t *xlst, Double_t *par ) {
    Double_t a = par[0]; 
    Double_t b = par[1]; 
    Double_t c = par[2];
    Double_t x = xlst[0];
    return funptr( a,b,c,x);
  }
  void fitphasespacecurve( TH1* hist, 
			   Double_t xmin, Double_t xmax, 
			   Double_t mA, Double_t mB, Double_t mC, Double_t mD, 
			   Double_t norm) 
  {
    Double_t normalization ; 
    Double_t eta; 
    
    normalization = (mB*mB - mA*mA) / (TMath::Sqrt(2.0) *mB ) ; 
    
    eta =TMath::ATanH ( TMath::Sqrt( 1.0 - 4.0 * mB* mB / (mD * mD) ) ); 
    
    TF1 *f1 = new TF1("myfunc", myfunc, xmin, xmax , 3 ); 
    
    f1->SetParNames("normalization","eta", "overall");
    f1->SetParameters( normalization, eta, norm ) ; 
    
  
    hist->Fit("myfunc"); 
    f1->Draw("same"); 
    
  }
  
  cout << "reading root file: " << filename <<  endl; 
  
  TFile f(filename); 

  TH1F* hist = (TH1F*)f.Get(histname) ;
  if (hist == 0 ) {
    cout << "no histogram with name : " << histname << endl; 
    return -1 ;
  }
  
  fitphasespacecurve( hist, 0, 2000, 1600, 700, 700, 0, 1.0 ); 

  TCanvas* c1 = new TCanvas( "mycanvas","mycanvas" ) ; 

  c1->cd(1); 
  c1->SetBorderMode(0); 
  c1->SetFillStyle(1001); 
  c1->SetFillColor(10);  

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
  return 0; 
}
*/


extern int testfunptr( double funptr( double ) )
{
  cout << "funptr (" << 3 << ") = " << funptr (3) << endl; 
  return 0; 
}
