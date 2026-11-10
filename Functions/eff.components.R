eff.components <- function(x1,y1,x2,y2,gx1,gy1,gx2,gy2) {
  # evaluate efficiency
  # CRS
  c11.B= Benchmarking::dea.direct(X=x1,Y=y1,
                                    DIREC=cbind(gx1,gy1),
                                    XREF=x1,YREF=y1,
                                    ORIENTATION="in-out",
                                    RTS="crs")$objval
  c12.B= Benchmarking::dea.direct(X=x1,Y=y1,
                                    DIREC=cbind(gx1,gy1),
                                    XREF=x2,YREF=y2,
                                    ORIENTATION="in-out",
                                    RTS="crs")$objval
  c21.B= Benchmarking::dea.direct(X=x2,Y=y2,
                                    DIREC=cbind(gx2,gy2),
                                    XREF=x1,YREF=y1,
                                    ORIENTATION="in-out",
                                    RTS="crs")$objval
  c22.B= Benchmarking::dea.direct(X=x2,Y=y2,
                                    DIREC=cbind(gx2,gy2),
                                    XREF=x2,YREF=y2,
                                    ORIENTATION="in-out",
                                    RTS="crs")$objval
  ## VRS
  v11.B= Benchmarking::dea.direct(X=x1,Y=y1,
                                    DIREC=cbind(gx1,gy1),
                                    XREF=x1,YREF=y1,
                                    ORIENTATION="in-out",
                                    RTS="vrs")$objval
  v12.B= Benchmarking::dea.direct(X=x1,Y=y1,
                                    DIREC=cbind(gx1,gy1),
                                    XREF=x2,YREF=y2,
                                    ORIENTATION="in-out",
                                    RTS="vrs")$objval
  v21.B= Benchmarking::dea.direct(X=x2,Y=y2,
                                    DIREC=cbind(gx2,gy2),
                                    XREF=x1,YREF=y1,
                                    ORIENTATION="in-out",
                                    RTS="vrs")$objval
  v22.B= Benchmarking::dea.direct(X=x2,Y=y2,
                                    DIREC=cbind(gx2,gy2),
                                    XREF=x2,YREF=y2,
                                    ORIENTATION="in-out",
                                    RTS="vrs")$objval
  # return the estimated efficiency
  res=list(c11=c11.B,c12=c12.B,c21=c21.B,c22=c22.B,
           v11=v11.B,v12=v12.B,v21=v21.B,v22=v22.B)
  #
  return(res)
}