
(** BLAS support *)

type order = 
  | RowMajor
  | ColMajor

type transpose =
  | NoTrans
  | Trans
  | ConjTrans

type uplo =
  | Upper
  | Lower

type diag =
  | NonUnit
  | Unit

type side =
  | Left
  | Right

open Gsl_matrix
open Gsl_vector

(** {3 LEVEL 1} *)

external dot : vector -> vector -> float
    = "ml_gsl_blas_ddot"

external nrm2 : vector -> float
    = "ml_gsl_blas_dnrm2"

external asum : vector -> float
    = "ml_gsl_blas_dasum"

external iamax : vector -> int
    = "ml_gsl_blas_idamax"

external swap : vector -> vector -> unit
    = "ml_gsl_blas_dswap"

external copy : vector -> vector -> unit
    = "ml_gsl_blas_dcopy"

external axpy : float -> vector -> vector -> unit
    = "ml_gsl_blas_daxpy"

external rot : vector -> vector -> float -> float -> unit
    = "ml_gsl_blas_drot"

external scal : float -> vector -> unit
    = "ml_gsl_blas_dscal"


(** {3 LEVEL 2} *)

external gemv : transpose -> alpha:float -> a:matrix -> 
  x:vector -> beta:float -> y:vector -> unit
      = "ml_gsl_blas_dgemv_bc" "ml_gsl_blas_dgemv"

external trmv : uplo -> transpose -> diag -> 
  a:matrix -> x:vector -> unit
      = "ml_gsl_blas_dtrmv"

external trsv : uplo -> transpose -> diag -> 
  a:matrix -> x:vector -> unit
      = "ml_gsl_blas_dtrsv"

external symv : uplo -> alpha:float -> a:matrix -> 
  x:vector -> beta:float -> y:vector -> unit
      = "ml_gsl_blas_dsymv_bc" "ml_gsl_blas_dsymv"

external dger : alpha:float -> x:vector -> 
  y:vector -> a:matrix -> unit
      = "ml_gsl_blas_dger"

external syr : uplo -> alpha:float -> x:vector -> 
  a:matrix -> unit
      = "ml_gsl_blas_dsyr"

external syr2 : uplo -> alpha:float -> x:vector -> 
  y:vector -> a:matrix -> unit
      = "ml_gsl_blas_dsyr2"


(** {3 LEVEL 3} *)

external gemm : ta:transpose -> tb:transpose ->
  alpha:float -> a:matrix -> b:matrix ->
    beta:float -> c:matrix -> unit
	= "ml_gsl_blas_dgemm_bc" "ml_gsl_blas_dgemm"

external symm : side -> uplo ->
  alpha:float -> a:matrix -> b:matrix ->
    beta:float -> c:matrix -> unit
	= "ml_gsl_blas_dsymm_bc" "ml_gsl_blas_dsymm"

external trmm : side -> uplo -> transpose -> diag -> 
  alpha:float -> a:matrix -> b:matrix -> unit
	= "ml_gsl_blas_dtrmm_bc" "ml_gsl_blas_dtrmm"

external trsm : side -> uplo -> transpose -> diag -> 
  alpha:float -> a:matrix -> b:matrix -> unit
	= "ml_gsl_blas_dtrsm_bc" "ml_gsl_blas_dtrsm"

external syrk : uplo -> transpose -> 
  alpha:float -> a:matrix -> beta:float -> c:matrix -> unit
	= "ml_gsl_blas_dsyrk_bc" "ml_gsl_blas_dsyrk"

external syr2k : uplo -> transpose -> 
  alpha:float -> a:matrix -> b:matrix -> 
    beta:float -> c:matrix -> unit
	= "ml_gsl_blas_dsyr2k_bc" "ml_gsl_blas_dsyr2k"


(** {3 Single precision} *)

open Gsl_vector.Single
open Gsl_matrix.Single

module Single =
  struct

  (** {4 LEVEL 1} *)

  external sdsdot : alpha:float -> vector -> vector -> float = "ml_gsl_blas_sdsdot"
  external dsdot : vector -> vector -> float = "ml_gsl_blas_dsdot"

  external dot  : vector -> vector -> float = "ml_gsl_blas_sdot"
  external nrm2 : vector -> float = "ml_gsl_blas_snrm2"
  external asum : vector -> float = "ml_gsl_blas_sasum"
  external iamax : vector -> int = "ml_gsl_blas_isamax"
  external swap : vector -> vector -> unit = "ml_gsl_blas_sswap"
  external copy : vector -> vector -> unit = "ml_gsl_blas_scopy"
  external axpy : float -> vector -> vector -> unit = "ml_gsl_blas_saxpy"
  external rot : vector -> vector -> float -> float -> unit = "ml_gsl_blas_srot"
  external scal : float -> vector -> unit = "ml_gsl_blas_sscal"

  (** {4 LEVEL 2} *)

  external gemv : transpose -> alpha:float -> a:matrix -> 
    x:vector -> beta:float -> y:vector -> unit
	= "ml_gsl_blas_sgemv_bc" "ml_gsl_blas_sgemv"
  external trmv : uplo -> transpose -> diag -> 
    a:matrix -> x:vector -> unit
	= "ml_gsl_blas_strmv"
  external trsv : uplo -> transpose -> diag -> 
    a:matrix -> x:vector -> unit
	= "ml_gsl_blas_strsv"
  external symv : uplo -> alpha:float -> a:matrix -> 
    x:vector -> beta:float -> y:vector -> unit
	= "ml_gsl_blas_ssymv_bc" "ml_gsl_blas_ssymv"
  external dger : alpha:float -> x:vector -> 
    y:vector -> a:matrix -> unit
	= "ml_gsl_blas_sger"
  external syr : uplo -> alpha:float -> x:vector -> 
    a:matrix -> unit
	= "ml_gsl_blas_ssyr"
  external syr2 : uplo -> alpha:float -> x:vector -> 
    y:vector -> a:matrix -> unit
	= "ml_gsl_blas_ssyr2"

  (** {4 LEVEL 3} *)

  external gemm : ta:transpose -> tb:transpose ->
    alpha:float -> a:matrix -> b:matrix ->
      beta:float -> c:matrix -> unit
	  = "ml_gsl_blas_sgemm_bc" "ml_gsl_blas_sgemm"
  external symm : side -> uplo ->
    alpha:float -> a:matrix -> b:matrix ->
      beta:float -> c:matrix -> unit
	  = "ml_gsl_blas_ssymm_bc" "ml_gsl_blas_ssymm"
  external syrk : uplo -> transpose -> 
    alpha:float -> a:matrix -> beta:float -> c:matrix -> unit
	= "ml_gsl_blas_ssyrk_bc" "ml_gsl_blas_ssyrk"
  external syr2k : uplo -> transpose -> 
    alpha:float -> a:matrix -> b:matrix -> 
      beta:float -> c:matrix -> unit
	  = "ml_gsl_blas_ssyr2k_bc" "ml_gsl_blas_ssyr2k"
  external trmm : side -> uplo -> transpose -> diag -> 
    alpha:float -> a:matrix -> b:matrix -> unit
	= "ml_gsl_blas_strmm_bc" "ml_gsl_blas_strmm"
  external trsm : side -> uplo -> transpose -> diag -> 
    alpha:float -> a:matrix -> b:matrix -> unit
	= "ml_gsl_blas_strsm_bc" "ml_gsl_blas_strsm"
  end


(** {3 Complex} *)
open Gsl_vector_complex
open Gsl_matrix_complex
open Gsl_complex

module Complex =
  struct

  (** {4 LEVEL 1} *)

  external dotu : vector -> vector -> complex = "ml_gsl_blas_zdotu"
  external dotc : vector -> vector -> complex = "ml_gsl_blas_zdotc"
  external nrm2 : vector -> float = "ml_gsl_blas_znrm2"
  external asum : vector -> float = "ml_gsl_blas_zasum"
  external iamax : vector -> int = "ml_gsl_blas_izamax"
  external swap : vector -> vector -> unit = "ml_gsl_blas_zswap"
  external copy : vector -> vector -> unit = "ml_gsl_blas_zcopy"
  external axpy : complex -> vector -> vector -> unit = "ml_gsl_blas_zaxpy"
  external scal : complex -> vector -> unit = "ml_gsl_blas_zscal"
  external zdscal : float -> vector -> unit = "ml_gsl_blas_zdscal"

  (** {4 LEVEL 2} *)

  external gemv : transpose -> alpha:complex -> a:matrix -> 
    x:vector -> beta:complex -> y:vector -> unit
	= "ml_gsl_blas_zgemv_bc" "ml_gsl_blas_zgemv"
  external trmv : uplo -> transpose -> diag -> 
    a:matrix -> x:vector -> unit
	= "ml_gsl_blas_ztrmv"
  external trsv : uplo -> transpose -> diag -> 
    a:matrix -> x:vector -> unit
	= "ml_gsl_blas_ztrsv"
  external hemv : uplo -> alpha:complex -> a:matrix -> 
    x:vector -> beta:complex -> y:vector -> unit
	= "ml_gsl_blas_zhemv_bc" "ml_gsl_blas_zhemv"
  external geru : alpha:complex -> x:vector -> 
    y:vector -> a:matrix -> unit
	= "ml_gsl_blas_zgeru"
  external gerc : alpha:complex -> x:vector -> 
    y:vector -> a:matrix -> unit
	= "ml_gsl_blas_zgerc"
  external her : uplo -> alpha:float -> 
    x:vector -> a:matrix -> unit
	= "ml_gsl_blas_zher"
  external her2 : uplo -> alpha:complex -> 
    x:vector -> y:vector -> a:matrix -> unit
	= "ml_gsl_blas_zher2"

  (** {4 LEVEL 3} *)

  external gemm : ta:transpose -> tb:transpose ->
    alpha:complex -> a:matrix -> b:matrix ->
      beta:complex -> c:matrix -> unit
	  = "ml_gsl_blas_zgemm_bc" "ml_gsl_blas_zgemm"
  external symm : side -> uplo ->
    alpha:complex -> a:matrix -> b:matrix ->
      beta:complex -> c:matrix -> unit
	  = "ml_gsl_blas_zsymm_bc" "ml_gsl_blas_zsymm"
  external syrk : uplo -> transpose -> 
    alpha:complex -> a:matrix -> beta:complex -> c:matrix -> unit
	= "ml_gsl_blas_zsyrk_bc" "ml_gsl_blas_zsyrk"
  external syr2k : uplo -> transpose -> 
    alpha:complex -> a:matrix -> b:matrix -> 
      beta:complex -> c:matrix -> unit
	  = "ml_gsl_blas_zsyr2k_bc" "ml_gsl_blas_zsyr2k"
  external trmm : side -> uplo -> transpose -> diag -> 
    alpha:complex -> a:matrix -> b:matrix -> unit
	= "ml_gsl_blas_ztrmm_bc" "ml_gsl_blas_ztrmm"
  external trsm : side -> uplo -> transpose -> diag -> 
    alpha:complex -> a:matrix -> b:matrix -> unit
	= "ml_gsl_blas_ztrsm_bc" "ml_gsl_blas_ztrsm"
  external hemm : side -> uplo -> alpha:complex -> 
    a:matrix -> b:matrix -> beta:complex -> c:matrix -> unit
	= "ml_gsl_blas_zhemm_bc" "ml_gsl_blas_zhemm"
  external herk : uplo -> transpose -> alpha:float -> 
    a:matrix -> beta:float -> c:matrix -> unit
	= "ml_gsl_blas_zherk_bc" "ml_gsl_blas_zherk"
  external her2k : uplo -> transpose -> alpha:complex -> 
    a:matrix -> b:matrix -> beta:float -> c:matrix -> unit
	= "ml_gsl_blas_zher2k_bc" "ml_gsl_blas_zher2k"
  end


(** {3 Complex single precision} *)

open Gsl_vector_complex.Single
open Gsl_matrix_complex.Single
open Gsl_complex

module Complex_Single =
  struct

  (** {4 LEVEL 1} *)

  external dotu : vector -> vector -> complex = "ml_gsl_blas_cdotu"
  external dotc : vector -> vector -> complex = "ml_gsl_blas_cdotc"
  external nrm2 : vector -> float = "ml_gsl_blas_scnrm2"
  external asum : vector -> float = "ml_gsl_blas_scasum"
  external iamax : vector -> int = "ml_gsl_blas_icamax"
  external swap : vector -> vector -> unit = "ml_gsl_blas_cswap"
  external copy : vector -> vector -> unit = "ml_gsl_blas_ccopy"
  external axpy : complex -> vector -> vector -> unit = "ml_gsl_blas_caxpy"
  external scal : complex -> vector -> unit = "ml_gsl_blas_cscal"
  external csscal : float -> vector -> unit = "ml_gsl_blas_csscal"

  (** {4 LEVEL 2} *)

  external gemv : transpose -> alpha:complex -> a:matrix -> 
    x:vector -> beta:complex -> y:vector -> unit
	= "ml_gsl_blas_cgemv_bc" "ml_gsl_blas_cgemv"
  external trmv : uplo -> transpose -> diag -> 
    a:matrix -> x:vector -> unit
	= "ml_gsl_blas_ctrmv"
  external trsv : uplo -> transpose -> diag -> 
    a:matrix -> x:vector -> unit
	= "ml_gsl_blas_ctrsv"
  external hemv : uplo -> alpha:complex -> a:matrix -> 
    x:vector -> beta:complex -> y:vector -> unit
	= "ml_gsl_blas_chemv_bc" "ml_gsl_blas_chemv"
  external geru : alpha:complex -> x:vector -> 
    y:vector -> a:matrix -> unit
	= "ml_gsl_blas_cgeru"
  external gerc : alpha:complex -> x:vector -> 
    y:vector -> a:matrix -> unit
	= "ml_gsl_blas_cgerc"
  external her : uplo -> alpha:float -> 
    x:vector -> a:matrix -> unit
	= "ml_gsl_blas_cher"
  external her2 : uplo -> alpha:complex -> 
    x:vector -> y:vector -> a:matrix -> unit
	= "ml_gsl_blas_cher2"

  (** {4 LEVEL 3} *)

  external gemm : ta:transpose -> tb:transpose ->
    alpha:complex -> a:matrix -> b:matrix ->
      beta:complex -> c:matrix -> unit
	  = "ml_gsl_blas_cgemm_bc" "ml_gsl_blas_cgemm"
  external symm : side -> uplo ->
    alpha:complex -> a:matrix -> b:matrix ->
      beta:complex -> c:matrix -> unit
	  = "ml_gsl_blas_csymm_bc" "ml_gsl_blas_csymm"
  external syrk : uplo -> transpose -> 
    alpha:complex -> a:matrix -> beta:complex -> c:matrix -> unit
	= "ml_gsl_blas_csyrk_bc" "ml_gsl_blas_csyrk"
  external syr2k : uplo -> transpose -> 
    alpha:complex -> a:matrix -> b:matrix -> 
      beta:complex -> c:matrix -> unit
	  = "ml_gsl_blas_csyr2k_bc" "ml_gsl_blas_csyr2k"
  external trmm : side -> uplo -> transpose -> diag -> 
    alpha:complex -> a:matrix -> b:matrix -> unit
	= "ml_gsl_blas_ctrmm_bc" "ml_gsl_blas_ctrmm"
  external trsm : side -> uplo -> transpose -> diag -> 
    alpha:complex -> a:matrix -> b:matrix -> unit
	= "ml_gsl_blas_ctrsm_bc" "ml_gsl_blas_ctrsm"
  external hemm : side -> uplo -> alpha:complex -> 
    a:matrix -> b:matrix -> beta:complex -> c:matrix -> unit
	= "ml_gsl_blas_chemm_bc" "ml_gsl_blas_chemm"
  external herk : uplo -> transpose -> alpha:float -> 
    a:matrix -> beta:float -> c:matrix -> unit
	= "ml_gsl_blas_cherk_bc" "ml_gsl_blas_cherk"
  external her2k : uplo -> transpose -> alpha:complex -> 
    a:matrix -> b:matrix -> beta:float -> c:matrix -> unit
	= "ml_gsl_blas_cher2k_bc" "ml_gsl_blas_cher2k"
  end
