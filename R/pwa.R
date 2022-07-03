#' Create pwa instance
#'
#' @description an instance with methods
#'
#' @param indexTag index page tag
#' @param publishingSite publishing site's url
#' @param folder local folder to contain the website
#' @param icon512 local file path to a 512x512 icon
#'
#' @return an pwa instance with properties $folder, $swjs, $webmanifest, and with methods $index() turning page_index() into an index page tag with pwa setup, $updateManifest(), $updateServiceWorker(), and $updateIcons(icon512).
#' @export
#'
create_pwa = function(indexTag=NULL, publishingSite, folder="docs", icon512=NULL){
  require(htmltools)
  if(!dir.exists(folder)) dir.create(folder)

  publishingSite |> generate_pwaStructure() -> pwa
  pwa$folder = folder
  html_factory(.tag=indexTag, pwa=pwa) |> save_html(file=file.path(folder,"index.html"))
  pwa$index = function(.tag){
    html_factory(.tag=.tag, pwa=pwa) |> save_html(file=file.path(pwa$folder,"index.html"))
  }
  pwa$swjs |> xfun::write_utf8(file.path(folder,"sw.js"))
  pwa$webmanifest |> xfun::write_utf8(file.path(folder,"manifest.webmanifest"))
  pwa |> read_manifest()
  pwa$updateManifest = updateManifest(pwa)
  pwa$updateServiceWorker = updateSW(pwa)
  if(!is.null(icon512)){
    generateIconsFrom512px(pwa, icon512)
  }
  pwa$updateIcons = function(icon512){
    generateIconsFrom512px(pwa, icon512)
  }
  if(any(search()=="pwaEnv")) detach("pwaEnv")
  pwaEnv=new.env()
  pwaEnv$pwa = pwa
  attach(pwaEnv)
  invisible(pwa)
}

read_manifest = function(pwa){
  pwa$webmanifest |> jsonlite::fromJSON(simplifyDataFrame = F) -> .json
  .json -> pwa$webmanifestJson
  invisible(.json)
}
generateIconsFrom512px= function(pwa, icon512){
  magick::image_read(icon512) -> img512
  magick::image_info(img512)  -> info
  if(info$width != 512L || info$height !=512){
    warning("image input should be 512x512 px.")
    stop()
  }

  folderpath= file.path(pwa$folder,"icons") #dirname(normalizePath(icon512))
  if(!dir.exists(folderpath)) dir.create(folderpath)
  imgname = basename(icon512)

  stringr::str_replace(
    imgname, "\\.", "-512."
  ) -> imgname512
  magick::image_write(img512, path=file.path(folderpath, imgname512))

  img192 = magick::image_scale(img512,
    magick::geometry_size_pixels(width=192, height=192))
  stringr::str_replace(
    imgname, "\\.", "-192."
  ) -> imgname192
  magick::image_write(img192, path=file.path(folderpath, imgname192))
  # also favicon
  img144 = magick::image_scale(img512,
    magick::geometry_size_pixels(width=144, height=144))
  stringr::str_replace(
    imgname, "\\.", "-144."
  ) -> imgname144
  magick::image_write(img144, path=file.path(folderpath, imgname144))
  magick::image_write(img144, path=file.path(folderpath, "favicon.png"))
  magick::image_write(img144, path=file.path(folderpath, "icon-144.png"))
  # apple-touch-icon
  img256 = magick::image_scale(img512,
    magick::geometry_size_pixels(width=256, height=256))
  magick::image_write(img256, path=file.path(folderpath, "apple-touch-icon.png"))
  pwa$icons = c(imgname512, imgname192, imgname144)

  pwa |> read_manifest()
  pwa |> updateManifestIcons()
  invisible(
    c(imgname512, imgname192, imgname144)
  )
}
updateManifest = function(pwa){
  function(){
    pwa$webmanifestJson |>
      jsonlite::toJSON(auto_unbox = T) |>
      xfun::write_utf8(con=file.path(pwa$folder, "manifest.webmanifest"))
  }
}
updateSW = function(pwa){
  function(file=""){
    if(file==""){xfun::write_utf8(pwa$swjs, con=file.path(pwa$folder, "sw.js"))} else {
      file.copy(from=file, to=file.path(pwa$folder, "sw.js"), overwrite = T)
    }
  }
}
updateManifestIcons <- function(pwa) {
  pwa$icons |> purrr::map(
    ~{
      size = stringr::str_extract(.x, "(?<=-)[0-9]+(?=\\.)")

      list(
        src=file.path("icons", .x),
        sizes=paste0(size,"x",size)
      )}
  ) -> pwa$webmanifestJson$icons
  pwa$webmanifestJson |>
    jsonlite::toJSON(auto_unbox = T) |>
    xfun::write_utf8(con=file.path(pwa$folder, "manifest.webmanifest"))
}
serviceWorker_factory=function(pwa){
  pwa$swjs |> xfun::write_utf8("sw.js")
}
manifest_factory = function(pwa){
  require(htmltools)
  pwa$webmanifest |>
    xfun::write_utf8("manifest.webmanifest")
}

html_factory = function(.tag=NULL, pwa){
  require(htmltools)
  tagList(
    tags$head(
      HTML(pwa$html$head)
    ),
    tags$body(
      HTML(pwa$html$body),
      .tag
    ))
}
generate_pwaStructure <- function(publishingSite) {
  if(!stringr::str_detect(publishingSite,"/$")){
    publishingSite=paste0(publishingSite, "/")
  }
  repo = paste0("/",basename(publishingSite))
  if(stringr::str_detect(publishingSite, glue::glue("https?:/{repo}"))){
    # repo is not actually publishingSite base url not a repo
    repo=""

  }
  pwa = new.env()
  pwa$repo = repo
  '<link rel="canonical" href="<<publishingSite>>" />
  <link rel="manifest" href="<<repo>>/manifest.webmanifest">' |>
    glue::glue(.open="<<", .close=">>") -> html_link_head

  "<script>
        if (navigator.serviceWorker) {
          navigator.serviceWorker.register (
            '<<repo>>/sw.js',
            {scope: '<<repo>>/'}
          )
        }
    </script>" |>
    glue::glue(.open="<<", .close=">>") -> html_script_body
  pwa$html = list(
    head=html_link_head,
    body=html_script_body
  )


  "// Change this to your repository name
  var GHPATH = '<<repo>>';

  // Choose a different app prefix name
  var APP_PREFIX = 'gppwa_';

  // The version of the cache. Every time you change any of the files
  // you need to change this version (version_01, version_02…).
  // If you donot change the version, the service worker will give your
  // users the old files!
  var VERSION = 'version_01';

  // The files to make available for offline use. make sure to add
  // others to this list
  var URLS = [
    `${GHPATH}/`,
    `${GHPATH}/index.html`
  ]

  var CACHE_NAME = APP_PREFIX + VERSION
self.addEventListener('fetch', function (e) {
  console.log('Fetch request : ' + e.request.url);
  e.respondWith(
    caches.match(e.request).then(function (request) {
      if (request) {
        console.log('Responding with cache : ' + e.request.url);
        return request
      } else {
        console.log('File is not cached, fetching : ' + e.request.url);
        return fetch(e.request)
      }
    })
  )
})

self.addEventListener('install', function (e) {
  e.waitUntil(
    caches.open(CACHE_NAME).then(function (cache) {
      console.log('Installing cache : ' + CACHE_NAME);
      return cache.addAll(URLS)
    })
  )
})

self.addEventListener('activate', function (e) {
  e.waitUntil(
    caches.keys().then(function (keyList) {
      var cacheWhitelist = keyList.filter(function (key) {
        return key.indexOf(APP_PREFIX)
      })
      cacheWhitelist.push(CACHE_NAME);
      return Promise.all(keyList.map(function (key, i) {
        if (cacheWhitelist.indexOf(key) === -1) {
          console.log('Deleting cache : ' + keyList[i] );
          return caches.delete(keyList[i])
        }
      }))
    })
  )
})
" |> glue::glue(.open="<<", .close=">>") -> offlineSetting_swjs
  pwa$swjs=offlineSetting_swjs

  '{
    // Name of the app and short name in case there isnot enough space
  "name": "Github Page PWA",
  "short_name": "GPPWA",
  // Description what your app is
  "description": "Github Page as a Progressive Web App",

  // Scope and start URL - these need to change to yours
  "scope": "<<repo>>/",
  "start_url": "<<repo>>/",

  // colours of the app as displayed in the installer
  "background_color": "#ffffff",
  "theme_color": "#ffffff",

  // Display of the app.
  //This could be "standalone", "fullscreen", "minimal-ui" or "browser"
  "display": "standalone",

  // The possible icons to display. Make sure to change the src URL,
  // the type and the size to your needs. If the size isnot correct,
    // you may not be able to install the app.
    "icons": [
        {
          "src": "<<repo>>/img/icon.png",
          "type": "image/png",
          "sizes": "700x700"
        }
    ]
  }' |> stringr::str_glue(.open="<<", .close=">>") -> webmanifest
  pwa$webmanifest=webmanifest
  pwa
}
