var init_upload = function(id, pathid) {
    $(id).off();
    $(id).on({
        dragover : function(e) {
            var dataTransfer = e.dataTransfer = e.originalEvent.dataTransfer;
            if (dataTransfer) {
                dataTransfer.dropEffect = 'copy';
            }
            e.preventDefault();
        },

        drop : function(e) {
            e.preventDefault();
            return false;
        }
    });

    upload = function(path) {
        $(obj(pathid)).val(path);
        $(obj(pathid)).trigger('change');

    };
};

var init_download = function(pathid) {
download = function (path) {
        $(obj(pathid)).val(path);
        $(obj(pathid)).trigger('change');
    }};

