local function build_project()
	local build_cmd = "Rscript testsAll.R 2>&1"
	local handle = io.popen(build_cmd)
	local result = handle:read("*a")
	local test = handle:close()
	if #result == 0 then
		build_cmd = "echo 'done' 2>&1"
		handle = io.popen(build_cmd)
		result = handle:read("*a")
		handle:close()
	else
	end
	local popup = require("popup")
	local Window = require("plenary.window")
	result = vim.split(result, "\n")
	local bufnr = vim.api.nvim_create_buf(false, false)
	vim.api.nvim_buf_set_option(bufnr, "modifiable", true)
	vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, result)
	local popup_options = {
		title = "Build Result",
		line = 10,
		col = 200,
		pos = "botleft",
		padding = { 300, 300, 300, 300 },
		border = { 1, 1, 1, 1 },
		minwidth = 150,
		minheight = 200,
	}

	if last_popup_id then
		Window.try_close(last_popup_id, false)
		if last_bufnr then
			vim.api.nvim_buf_delete(last_bufnr, { force = true })
		end
	end
	last_bufnr = bufnr

	last_popup_id = popup.create(bufnr, popup_options)
	if vim.api.nvim_buf_is_valid(bufnr) then
		vim.api.nvim_buf_set_option(bufnr, "filetype", "cpp")
	end
end

build_project()
