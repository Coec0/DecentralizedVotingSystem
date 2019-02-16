module.exports = {
	root: true,
	env: {
		node: true
	},
	extends: ['plugin:vue/essential'],
	rules: {
		'no-console': process.env.NODE_ENV === 'production' ? 'error' : 'off',
		'no-debugger': process.env.NODE_ENV === 'production' ? 'error' : 'off',
		'vue/script-indent': ['error', 'tab', { baseIndent: 0 }]
	},
	parserOptions: {
		parser: 'babel-eslint'
	}
};
