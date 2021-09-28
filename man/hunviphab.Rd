\name{hello}
\alias{hello}
\title{Hello, World!}
\usage{
hello()
}
\description{
Prints 'Hello, world!'.
}
\examples{
hello()
}

\name{wkt2coords}
\alias{wkt2coords}
\title{wkt2coords}
\usage{
wkt2coords()
}
\description{
Return coordinates from WKT geometry.
}
\examples{
df <- cbind(df,wkt2coords(df,df$obm_geometry,4326,23700))
}
