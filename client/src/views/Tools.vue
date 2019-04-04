<template>
	<div class="tools container flex-grow-1">
		<whitelist></whitelist>
		<encrypt-privatekey></encrypt-privatekey>
	</div>
</template>

<script>
import Whitelist from '@/components/Tools/Whitelist.vue';
import EncryptPrivateKey from '@/components/Tools/EncryptPrivateKey.vue';

export default {
	name: 'Tools',
	components: {
		'whitelist': Whitelist,
		'encrypt-privatekey': EncryptPrivateKey
	}
};
</script>

<style scoped>
h1 {
	font-size: 5rem;
	margin-top: 1em;
	margin-bottom: 1em;
}

.tools {
	padding-top: 50px;
	display: flex;
	flex-wrap: wrap;
	justify-content: center;
	align-content: flex-start;
}

.container div {
	  transition: box-shadow .3s;
	  margin: 25px;
	  border-radius:10px;
	  border: 1px solid #ccc;
	  background: #fff;
}

.container div:hover {
  box-shadow: 0 0 11px rgba(33,33,33,.2); 
}
</style>
