(* ::Package:: *)

BeginPackage["RSK`"];
(* usages go here *)
RSK::usage = "Full RSK";
RSK1::usage = "RSK first part";
RSK2::usage = "RSK second part";

t;

Begin ["Private`"];
(* Function definitions go here *)
RSK1[A_,W_] := (
	n=Length[W];
	pRSK1[A,W,Array[t,n]]
)

pRSK1[A_,W_,T_] := (
	n=Length[A];
	ww=W; TT=T; 
	For[m=Length[W],m>0,m--,
		tp=T;
		tc=T;
		Q=Position[ww,ww[[m]]];
		tp[[m]]=TT[[m]];
		TT[[m]]=Total[TT[[Flatten[Q]]]];
		For[k=Length[Q]-2,k>=0,k--,
			l=Flatten[Q[[k+1]]][[1]];
			ll=Flatten[Q[[k+2]]][[1]];
			tp[[l]]=TT[[l]]+tp[[ll]];
			tc[[l]]=TT[[l]]/(tp[[ll]]*(tp[[ll]]+TT[[l]]));
	
			For[s=l+1,s<ll,s++,
				tc[[s]]=TT[[s]]*((tp[[ll]])^(-A[[ ww[[ll]],ww[[s]] ]]))
			];
		];
		l=If[Length[Q]==1, Q[[1]][[1]], l];
		For[s=l-1, s>0,s--,
			tc[[s]]=TT[[s]]*((tp[[l]])^(-A[[ww[[l]],ww[[s]] ]]))
		];
		For[i=1,i<m,i++,TT[[i]]=tc[[i]] ];
		ww=Delete[ww,m];
\.10	];
	(*TT=Simplify[TT,TimeConstraint->1000000];*)
	TT
)

RSK2[A_,W_] := (
	n=Length[W];
	pRSK2[A,W,Array[t,n]]
)

pRSK2[A_,W_,T_]:= (
	n=Length[A];
	ww=W;
	TT=T;
	For[m=Length[W],m>0,m--,
		tp=TT;
		tc=TT;
		Q=Position[ww,ww[[m]]];
		tp[[m]]=TT[[m]];
		xm=TT[[m]];
		TT[[m]]=1/TT[[m]];
		tc[[m]]=TT[[m]];
		For[k=Length[Q]-1,k>0\.10,k--,
			l=If[k>0,Flatten[Q[[k]]][[1]],0];
			ll=Flatten[Q[[k+1]]][[1]];
			tc[[l]]=TT[[l]];
			r=tc[[ll]];	
			For[s=l+1,s<ll,s++,
				r=r*(TT[[s]]^(-A[[ww[[s]],ww[[l]]]]));
				tp[[s]]=TT[[s]];
			];
			tc[[l]]=TT[[l]]/(r*(r+TT[[ll]]));
			tp[[l]]=TT[[l]]+r;
		];
		For[i=1,i<m,i++,TT[[i]]=tp[[i]] ];
		TT[[m]]=xm;
	ww=Delete[ww,m];
	];
	(*TT=Simplify[TT,TimeConstraint->1000000];*)
	TT
)

RSK[A_,W_]:= (
	n=Length[W];
	pRSK2[A,W,pRSK1[A,W,Array[t,n]]]
)

End[]; (* private *)
(* protect what you want *)




EndPackage[];
